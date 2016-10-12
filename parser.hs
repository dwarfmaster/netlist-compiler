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
import Control.Monad
import System.IO
import System.Exit

parseFile :: String -> IO ([String],[String],[(String,Int64)],[GEq String])
parseFile file = parseFromFile parser file >>= either report return
 where report err  = do
           hPutStrLn stderr $ "Error: " ++ show err
           exitFailure

parseNET :: String -> IO Program
parseNET file = do
    (inpt,outpt,vrs,equations) <- parseFile file
    let (mp,cnt) = foldr (\(s,t) -> \(m,c) -> (M.insert s (c, totype t) m, c+1)) (M.empty,0) vrs
    let eqs      = map (fmap $ getidx mp) equations
    let inpts    = map (getidx mp) inpt
    let outpts   = map (getidx mp) outpt
    let types    = A.array (0,cnt-1) $ map ((get mp) . fst) vrs
    return $ Prog {p_eqs = eqs, p_inputs = inpts, p_outputs = outpts, p_types = types}
 where totype t = if t == 1 then TByte else TNap t
       getidx mp s = let Just (x,_) = M.lookup s mp in x
       get mp s    = let Just p = M.lookup s mp in p

parser :: Parser ([String],[String],[(String,Int64)],[GEq String])
parser = do
    mayWhite
    ipt <- wp input
    out <- wp output
    vrs <- wp vars
    eqs <- wp equas
    mayWhite >> eof
    return (ipt,out,vrs,eqs)

input  = string "INPUT" >-> commaSeparated varia
output = string "OUTPUT" >-> commaSeparated varia
vars   = string "VAR" >-> commaSeparated variatype
equas  = string "IN" >-> (many $ wp one_eq)
one_eq = do v <- wp varia
            wp $ char '='
            e <- expre
            return $ GEq v e

varia = word
value = number
arg   = (varia >>= \v -> return $ Avar v) <|> (value >>= \v -> return $ Aconst v)
binop =     strpat "OR"   Or
        <|> strpat "XOR"  Xor
        <|> strpat "AND"  And
        <|> strpat "NAND" Nand
expre =
     try (string "REG" >-> varia >>= (return . Ereg))
 <|> try (string "NOT" >-> arg >>= (return . Enot))
 <|> (try $ do b <- wp binop
               a1 <- wp arg
               a2 <- wp arg
               return $ Ebinop b a1 a2)
 <|> (try $ do wp $ string "EMUX"
               a1 <- wp arg
               a2 <- wp arg
               a3 <- wp arg
               return $ Emux a1 a2 a3)
 <|> (try $ do wp $ string "ROM"
               n1 <- wp number
               n2 <- wp number
               a  <- wp arg
               return $ Erom n1 n2 a)
 <|> (try $ do wp $ string "RAM"
               n1 <- wp number
               n2 <- wp number
               a1 <- wp arg
               a2 <- wp arg
               a3 <- wp arg
               a4 <- wp arg
               return $ Eram n1 n2 a1 a2 a3 a4)
 <|> (try $ do wp $ string "CONCAT"
               a1 <- wp arg
               a2 <- wp arg
               return $ Econcat a1 a2)
 <|> (try $ do wp $ string "SLICE"
               n1 <- wp number
               n2 <- wp number
               a  <- wp arg
               return $ Eslice n1 n2 a)
 <|> (try $ do wp $ string "SELECT"
               n <- wp number
               a <- wp arg
               return $ Eselect n a)
 <|> (arg >>= (return . Earg))

variatype = (try $ do
    v <- wp varia
    wp $ char ':'
    n <- wp number
    return (v,n))
 <|> (varia >>= (\v -> return (v,1)))
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

