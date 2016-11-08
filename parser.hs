{-# LANGUAGE FlexibleContexts #-}

module Parser (parseNET) where
import AST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Int
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Array.IArray as A
import Data.Array.IArray ((!))
import Control.Monad
import System.IO
import System.Exit

parseNET :: String -> IO Program
parseNET file = parseFromFile parser file >>= either report return
 where report err  = do
           hPutStrLn stderr $ "Error: " ++ show err
           exitFailure

check_unique :: [Int] -> [Int] -> Parser ()
check_unique inpts eqs = if not $ null pin
                         then fail $ "Input variable " ++ show hpi ++ " is computed again in equation"
                         else if not $ null peqs
                         then fail $ "Variable " ++ show hpe ++ " is computed twice"
                         else return ()
 where deqs = [ (x,length [ y | y <- eqs, y == x ]) | x <- eqs ]
       din  = [(x,y) | x <- eqs, y <- inpts]
       peqs = map fst $ filter (\(_,n) -> n > 1)  deqs
       pin  = map fst $ filter (\(a,b) -> a == b) din
       hpe  = head peqs
       hpi  = head pin

parser :: Parser Program
parser = do
    mayWhite
    ipt <- wp input
    out <- wp output
    vrs <- wp vars
    let (mp,cnt) = foldr (\(s,t) -> \(m,c) -> (M.insert s (c, t) m, c+1)) (M.empty,0) vrs
    let types    = A.array (0,cnt-1) $ map ((get mp) . fst) vrs
    inpts  <- mapM (getidx mp "input")  ipt
    outpts <- mapM (getidx mp "output") out
    eqs <- wp $ equas mp types
    mayWhite >> eof
    check_unique inpts $ map fst eqs
    return $ Prog { p_eqs = eqs, p_inputs = inpts, p_outputs = outpts, p_types = types }
 where get mp s = let Just p = M.lookup s mp in p

getidx mp tp s = let l = M.lookup s mp in case l of
                    Just (x,_) -> return x
                    Nothing    -> fail (s ++ " " ++ tp ++ " variable not declared in vars")

input  = string "INPUT" >-> commaSeparated varia
output = string "OUTPUT" >-> commaSeparated varia
vars   = string "VAR" >-> commaSeparated variatype
equas mp tps  = string "IN" >-> (many $ wp $ one_eq mp tps)
one_eq mp tps = do v <- wp varia
                   i <- getidx mp "result" v
                   wp $ char '='
                   e <- expre mp tps (tps ! i)
                   return $ (i, e)

varia = word
value = number
binop =     strpat "OR"   Or
        <|> strpat "XOR"  Xor
        <|> strpat "AND"  And
        <|> strpat "NAND" Nand

varia_constraint mp tps t = do
    v <- varia
    i <- getidx mp "member" v
    if (tps ! i) == t then return i else fail (v ++ " : type " ++ show (tps ! i) ++ ", expecting " ++ show t)

log2 x = ceiling $ logBase 2 $ fromIntegral x

value_constraint t = value >>= (\v -> let s = log2 v in if s <= size t then return v
    else fail ("Value " ++ show v ++ " incompatible with type " ++ show t))
 where size (TNap i) = i
       size TBit    = 1
arg_constraint mp tps t = (varia_constraint mp tps t >>= \v -> return $ Avar v)
                          <|> (value_constraint t >>= \v -> return $ Aconst v)

varia_exists mp = varia >>= getidx mp "member"
arg_exists mp   = (varia_exists mp >>= \v -> return $ Avar v) <|> (value >>= \v -> return $ Aconst v)

expre :: M.Map String (Int,Type) -> A.Array Int Type -> Type -> Parser Exp
expre mp tps t =
     try (string "REG" >-> variac >>= (return . Ereg))
 <|> try (string "NOT" >-> arg_exists mp >>= (return . Enot))
 <|> (try $ do b <- wp binop
               a1 <- wp argc
               a2 <- wp argc
               return $ Ebinop b a1 a2)
 <|> (try $ do wp $ string "EMUX"
               a1 <- wp argc
               a2 <- wp argc
               a3 <- wp $ arg_constraint mp tps TBit
               return $ Emux a1 a2 a3)
 <|> (try $ do wp $ string "ROM"
               n1 <- wp number
               n2 <- wp number
               if not $ isValidSize n2 then fail "ROM word size must be a power of 2" else return ()
               if size t /= n2 then fail ("Reading " ++ show n2 ++ " bits from ROM into a " ++ show t) else return ()
               a  <- wp $ arg_constraint mp tps $ TNap n1
               return $ Erom n1 n2 a)
 <|> (try $ do wp $ string "RAM"
               n1 <- wp number
               n2 <- wp number
               if not $ isValidSize n2 then fail "RAM word size must be a power of two" else return ()
               if size t /= n2 then fail ("Reading " ++ show n2 ++ " bits from RAM into a " ++ show t) else return ()
               a1 <- wp $ arg_constraint mp tps $ TNap n1
               a2 <- wp $ arg_constraint mp tps TBit
               a3 <- wp $ arg_constraint mp tps $ TNap n1
               a4 <- wp $ arg_constraint mp tps $ TNap n2
               return $ Eram n1 n2 a1 a2 a3 a4)
 <|> (try $ do wp $ string "CONCAT"
               a1 <- wp $ arg_exists mp
               a2 <- wp $ arg_exists mp
               if size (tt a1) + size (tt a2) /= size t then
                   fail ("Concat of size " ++ show (size (tt a1) + size (tt a2)) ++ " into a " ++ show t)
               else return ()
               return $ Econcat a1 a2)
 <|> (try $ do wp $ string "SLICE"
               n1 <- wp number
               n2 <- wp number
               a  <- wp $ arg_exists mp
               if n1 > n2 then fail ("Beggining of slice is after end of slice") else return ()
               if n2 >= size (tt a) then fail ("Cannot slice up to " ++ show n2 ++ " in a " ++ stt a) else return ()
               if size t /= (n2 - n1 + 1) then fail ("Slicing of size " ++ show (n2 - n1 + 1) ++ " into a " ++ show t)
                                          else return ()
               return $ Eslice n1 n2 a)
 <|> (try $ do wp $ string "SELECT"
               if t /= TBit then fail ("Cannot select into a nap") else return ()
               n <- wp number
               a <- wp $ arg_exists mp
               if n >= size (tt a) then fail ("Cannot select the " ++ show n ++ "th element of a " ++ stt a) else return ()
               return $ Eselect n a)
 <|> (argc >>= (return . Earg))
 where variac = varia_constraint mp tps t
       valuec = value_constraint t
       argc   = arg_constraint mp tps t
       size (TNap i) = i
       size TBit     = 1
       tt (Avar v)   = tps ! v
       tt (Aconst v) = TNap $ log2 v
       stt (Avar v)  = show $ tps ! v
       stt a         = show a
       isValidSize n =  n == 1  || n == 2  || n == 4  || n == 8
                     || n == 16 || n == 32 || n == 64

variatype = (try $ do
    v <- wp varia
    wp $ char ':'
    n <- wp number
    if n > 64 then fail "Naps can only be of length 63 or less" else return ()
    return $ if n == 1 then (v, TBit) else (v,TNap n))
 <|> (varia >>= (\v -> return (v,TBit)))
commaSeparated p = p `sepBy` (try $ mayWhite >> char ',' >> mayWhite)

(>->) x y = x >> whiteSpaces >> y

word :: Parser String
word = do c <- choice [lower, char '_']
          e <- many $ choice [lower, digit, char '_']
          return $ c : e
number :: Parser Int64
number = (liftM read) $ many1 digit
strpat :: String -> a -> Parser a
strpat str pat = string str >> return pat
wp p = p >>= (\v -> mayWhite >> return v)
whiteSpaces :: Parser ()
whiteSpaces = skipMany1 space
mayWhite :: Parser ()
mayWhite = skipMany space

