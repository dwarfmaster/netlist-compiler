
module Compil (compile) where
import AST
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.PatriciaTree as GPT
import qualified Data.List as L
import qualified Data.Array.IArray as A
import Data.Array.IArray ((!))
import qualified Data.ByteString as BS
import Data.Int
import Data.Maybe
import Data.IORef
import Control.Exception.Base
import Control.Monad

type Gr = GPT.Gr Equation ()
type Write = (Int64,Arg,Arg,Arg) -- word_size, we, wa, ra
fac = 8

type Labeler = IO String
nlabel_init :: IO Labeler
nlabel_init = newIORef 0 >>= (\r -> return $ do
    v <- readIORef r
    writeIORef r (v+1)
    return $ "_label_" ++ show v)

infixr 9 <:
(<:) :: Arg -> [Int] -> [Int]
(Avar v) <: l = v : l
_        <: l = l

mkGraph :: Program -> Gr
mkGraph p = G.mkGraph nds edgs
 where nds  = map (\(u,v) -> (u, (u,v))) $ p_eqs p
       edgs = [(v1,v2,()) | (v1,e) <- nds, v2 <- dep $ snd e]
       dep (Earg a)           = a  <: []
       dep (Enot a)           = a  <: []
       dep (Ebinop _ a1 a2)   = a1 <: a2 <: []
       dep (Emux a1 a2 a3)    = a1 <: a2 <: a3 <: []
       dep (Erom _ _ a)       = a  <: []
       dep (Eram _ _ a _ _ _) = a  <: []
       dep (Econcat a1 a2)    = a1 <: a2 <: []
       dep (Eslice _ _ a)     = a  <: []
       dep (Eselect _ a)      = a  <: []
       dep _                  = []

possibles g = filter ((==0) . (G.outdeg g) . fst) $ G.labNodes g

has_cycle g = foldl (||) False $ map ((/=1) . length) $ G.scc g

compile :: Program -> IO ()
compile p = do
    label <- nlabel_init
    if has_cycle g then fail "Cyclic dependencies" else return ()
    putStrLn ".text"
    putStrLn "print_int:"
    putStrLn "movq %rdi, %rsi"
    putStrLn "movq $message, %rdi"
    putStrLn "movq $0, %rax"
    putStrLn "call printf"
    putStrLn "ret"
    putStrLn "read_int:"
    putStrLn "movq %rdi, %rsi"
    putStrLn "movq $inmsg, %rdi"
    putStrLn "movq $0, %rax"
    putStrLn "call scanf"
    putStrLn "ret"

    putStrLn ".globl main"
    putStrLn "main:"
    putStrLn "movq %rsp, %rbp"
    putStrLn "subq $32, %rsp"
    -- TODO test the number of arguments
    -- TODO store size of rom and ram
    putStrLn "movq %rsi, %rbx"
    putStrLn "movq 8(%rbx), %rdi"  -- argv[1]
    putStrLn "call load_rom"
    putStrLn $ "movq %rax, -" ++ show (fac*2) ++ "(%rbp)"
    putStrLn "movq 16(%rbx), %rdi" -- argv[2]
    putStrLn "call load_ram"
    putStrLn $ "movq %rax, (%rbp)"

    putStrLn "movq %rsp, %rax"
    putStrLn "movq %rsp, %rbx"
    putStrLn $ "subq $" ++ show (8  * n) ++ ", %rbx"
    putStrLn $ "subq $" ++ show (16 * n) ++ ", %rsp"

    -- Init values at 0
    mapM_ (\v -> putStrLn $ "movq $0, -" ++ show (fac * v) ++ "(%rax)") [0..(n-1)]

    -- Mainloop
    putStrLn "mainloop:"

    putStrLn "pushq %rax"
    mapM_ inputV $ p_inputs p
    putStrLn "popq %rax"
    wrs <- mapM (writeE p n label) o
    putStrLn "pushq %rax"
    mapM_ (outputV p) $ p_outputs p
    putStrLn "popq %rax"

    let nwrs = map fromJust $ filter isJust wrs
    mapM_ (applyWrite label) nwrs

    putStrLn "movq %rax, %rcx"
    putStrLn "movq %rbx, %rax"
    putStrLn "movq %rcx, %rbx"
    putStrLn "jmp mainloop"

    putStrLn ".data"
    putStrLn "message:.string \"%llx\\n\""
    putStrLn "inmsg:.string \"%llx\""
 where g = mkGraph p
       o = L.reverse $ G.topsort' g
       n = (+) 1 $ snd $ A.bounds $ p_types p

applyWrite :: Labeler -> Write -> IO ()
applyWrite lb (ws,we,wa,dt) = do
    l <- lb
    readV we "rcx"
    putStrLn "test %rcx, %rcx"
    putStrLn $ "je " ++ l
    readV wa "rcx"
    putStrLn "salq $2, %rcx"
    readV dt "rdx"
    putStrLn $ "movq (%rbp), %rdi"
    putStrLn $ "movq %rdx, (%rdi, %rcx, "
               ++ show (ws `div` 4) ++ ")"
    putStrLn $ l ++ ":"

inputV :: Var -> IO ()
inputV v = do
    putStrLn $ "leaq -" ++ show (fac*v) ++ "(%rbx), %rdi"
    putStrLn "pushq %rbx"
    putStrLn "call read_int"
    putStrLn "popq %rbx"

outputV :: Program -> Var -> IO ()
outputV p v = do
    putStrLn $ "movq -" ++ show (fac*v) ++ "(%rbx), %rdi"
    if s < 32 then putStrLn $ "andq $" ++ show x ++ ", %rdi"
    else if s < 64 then do
        putStrLn $ "movq $1, %rcx"
        putStrLn $ "salq $" ++ show s ++ ", %rcx"
        putStrLn $ "decq %rcx"
        putStrLn $ "andq %rcx, %rdi"
    else return ()
    putStrLn "pushq %rbx"
    putStrLn "call print_int"
    putStrLn "popq %rbx"
 where s = size (p_types p) (Avar v)
       x = 2 ^ s - 1

writeE :: Program -> Int -> Labeler -> Equation -> IO (Maybe Write)
writeE p n lb (v,e) = do
    w <- writeExp p e lb
    putStrLn $ "movq %rdi, -" ++ show (fac*v) ++ "(%rbx)"
    return w

readV :: Arg -> String -> IO ()
readV (Aconst c) s = putStrLn $ "movq $" ++ show c ++ ", %" ++ s
readV (Avar v)   s = putStrLn $ "movq -" ++ show (fac*v) ++ "(%rbx), %" ++ s
readOV :: Arg -> String -> IO ()
readOV (Aconst c) s = putStrLn $ "movq $" ++ show c ++ ", %" ++ s
readOV (Avar v)   s = putStrLn $ "movq -" ++ show (fac*v) ++ "(%rax), %" ++ s

   -- Uses rdx after d and r, writes in rdi
readMemory :: Labeler -> String -> Int64 -> String -> IO ()
readMemory lb r 1  d = do
    putStrLn $ "movq %" ++ d ++ ", %rdi"
    putStrLn $ "movq %" ++ r ++ ", %rdx"
    putStrLn $ "shrq $3, %" ++ r
    putStrLn $ "andq $7, %rdx"
    putStrLn $ "movb (%rdi,%" ++ r ++ ",1), %dil"
    l1 <- lb
    l2 <- lb
    putStrLn $ "jmp " ++ l2
    putStrLn $ l1 ++ ":"
    putStrLn $ "shrq $1, %rdi"
    putStrLn $ "decq %rdx"
    putStrLn $ l2 ++ ":"
    putStrLn $ "andq %rdx,%rdx"
    putStrLn $ "jne " ++ l1
readMemory lb r 2  d = do
    putStrLn $ "movq %" ++ d ++ ", %rdi"
    putStrLn $ "movq %" ++ r ++ ", %rdx"
    putStrLn $ "shrq $2, %" ++ r
    putStrLn $ "andq $3, %rdx"
    putStrLn $ "movb (%rdi,%" ++ r ++ ",1), %dil"
    l1 <- lb
    l2 <- lb
    putStrLn $ "jmp " ++ l2
    putStrLn $ l1 ++ ":"
    putStrLn $ "shrq $2, %rdi"
    putStrLn $ "decq %rdx"
    putStrLn $ l2 ++ ":"
    putStrLn $ "andq %rdx,%rdx"
    putStrLn $ "jne " ++ l1
readMemory lb r 4  d = do
    putStrLn $ "movq %" ++ d ++ ", %rdi"
    putStrLn $ "movq %" ++ r ++ ", %rdx"
    putStrLn $ "shrq $1, %" ++ r
    putStrLn $ "andq $1, %rdx"
    putStrLn $ "movb (%rdi,%" ++ r ++ ",1), %dil"
    l1 <- lb
    l2 <- lb
    putStrLn $ "jmp " ++ l2
    putStrLn $ l1 ++ ":"
    putStrLn $ "shrq $4, %rdi"
    putStrLn $ "decq %rdx"
    putStrLn $ l2 ++ ":"
    putStrLn $ "andq %rdx,%rdx"
    putStrLn $ "jne " ++ l1
readMemory _ r 8  d =
    putStrLn $ "movq (%" ++ d ++ ",%"
                         ++ r ++ ",1), %rdi"
readMemory _ r 16 d = do
    putStrLn $ "salq $1, %" ++ r
    putStrLn $ "movw (%" ++ d ++ ",%rcx,1), %di"
readMemory _ r 32 d = do
    putStrLn $ "salq $2, %" ++ r
    putStrLn $ "movl (%" ++ d ++ ",%rcx,1), %edi"
readMemory _ r 64 d = do
    putStrLn $ "salq $3, %" ++ r
    putStrLn $ "movq (%" ++ d ++ ",%rcx,1), %rdi"

-- Uses rcx, rdx
-- Returns in rdi
writeExp :: Program -> Exp -> Labeler -> IO (Maybe Write)
writeExp _ (Earg a) _ = readV a "rdi" >> return Nothing

writeExp _ (Ereg v) _ = do
    putStrLn $ "movq -" ++ show (fac*v) ++ "(%rax), %rdi"
    return Nothing

writeExp _ (Enot a) _ = readV a "rdi" >> putStrLn "notq %rdi"
                        >> return Nothing

writeExp _ (Ebinop b a1 a2) _ = do
    readV a1 "rcx"
    readV a2 "rdi"
    case b of
        Or   -> putStrLn "orq %rcx, %rdi"
        Xor  -> putStrLn "xorq %rcx, %rdi"
        And  -> putStrLn "andq %rcx, %rdi"
        Nand -> putStrLn "andq %rcx, %rdi" >> putStrLn "not %rdi"
    return Nothing

writeExp _ (Emux a1 a2 a3) _ = do
    readV a3 "rcx"
    readV a1 "rdx"
    readV a2 "rdi"
    putStrLn "andq $1, %rcx"
    putStrLn "cmovqe %rdx, %rdi"
    return Nothing

-- TODO check overflow
writeExp p (Erom _ w r) lb = do
    readV r "rcx"
    putStrLn $ "movq -" ++ show (fac * 2) ++ "(%rbp), %rdx"
    readMemory lb "rcx" w "rdx"
    return Nothing
 where n = (+) 1 $ snd $ A.bounds $ p_types p

writeExp _ (Eram _ w ra we wa dt) lb = do
    readV ra "rcx"
    putStrLn $ "movq (%rbp), %rdx"
    readMemory lb "rcx" w "rdx"
    return $ Just (w, we, wa, dt)

writeExp p (Econcat a1 a2) _ = do
    readV a2 "rcx"
    readV a1 "rdi"
    let s = size (p_types p) a1
    putStrLn $ "salq $" ++ show s ++ ", %rcx"
    putStrLn $ "andq $" ++ show (2^s-1) ++ ", %rdi"
    putStrLn "or %rcx, %rdi"
    return Nothing

writeExp _ (Eslice i1 i2 a) _ = do
    readV a "rdi"
    putStrLn $ "shrq $" ++ show i1 ++ ", %rdi"
    return Nothing

writeExp _ (Eselect i a) _ = do
    readV a "rdi"
    putStrLn $ "shrq $" ++ show i ++ ", %rdi"
    return Nothing

size :: A.Array Int Type -> Arg -> Int64
size tps (Avar v)   = case (tps ! v) of
    TBit   -> 1
    TNap n -> n
size _   (Aconst c) = ceiling $ logBase 2 $ fromIntegral c

