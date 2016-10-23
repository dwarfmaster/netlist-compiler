
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
import Control.Exception.Base
import Control.Monad

type Gr = GPT.Gr Equation ()
type Write = (Int64,Arg,Arg,Arg) -- word_size, we, wa, ra
fac = 8

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
    putStrLn $ "subq $" ++ show (4*fac) ++ ", %rax"
    putStrLn $ "subq $" ++ show (fac * (4 + n)) ++ ", %rbx"
    putStrLn $ "subq $" ++ show (fac * (4+2*n)) ++ ", %rsp"

    -- Init values at 0
    mapM_ (\v -> putStrLn $ "movq $0, -" ++ show (fac * v) ++ "(%rax)") [0..(n-1)]

    -- Mainloop
    putStrLn "mainloop:"

    putStrLn "pushq %rax"
    mapM_ inputV $ p_inputs p
    putStrLn "popq %rax"
    wrs <- mapM (writeE p n) o
    putStrLn "pushq %rax"
    mapM_ outputV $ p_outputs p
    putStrLn "popq %rax"

    let nwrs = map fromJust $ filter isJust wrs
    foldM_ (\c w -> applyWrite w c >> return (c+1)) 0 nwrs

    putStrLn "movq %rax, %rcx"
    putStrLn "movq %rbx, %rax"
    putStrLn "movq %rcx, %rbx"
    putStrLn "jmp mainloop"

    putStrLn ".data"
    putStrLn "message:.string \"%d\\n\""
    putStrLn "inmsg:.string \"%u\""
    putStrLn "input:.quad"
 where g = mkGraph p
       o = L.reverse $ G.topsort' g
       n = (+) 1 $ snd $ A.bounds $ p_types p

applyWrite :: Write -> Int -> IO ()
applyWrite (ws,we,wa,dt) c = do
    readV we "rcx"
    putStrLn "test we, we"
    putStrLn $ "je _ramw_" ++ show c
    readV wa "rcx"
    putStrLn "salq 2, %rcx"
    readV dt "rdx"
    putStrLn $ "movq %rdx, 0(%rbp, %rcx, "
               ++ show (ws `div` 4) ++ ")"
    putStrLn $ "_ram_" ++ show c ++ ":"

inputV :: Var -> IO ()
inputV v = do
    putStrLn $ "leaq -" ++ show (fac*v) ++ "(%rbx), %rdi"
    putStrLn "pushq %rbx"
    putStrLn "call read_int"
    putStrLn "popq %rbx"

outputV :: Var -> IO ()
outputV v = do
    putStrLn $ "movq -" ++ show (fac*v) ++ "(%rbx), %rdi"
    putStrLn "pushq %rbx"
    putStrLn "call print_int"
    putStrLn "popq %rbx"

writeE :: Program -> Int -> Equation -> IO (Maybe Write)
writeE p n (v,e) = do
    w <- writeExp p e
    putStrLn $ "andq $" ++ show x ++ ", %rdi"
    putStrLn $ "movq %rdi, -" ++ show (fac*v) ++ "(%rbx)"
    return w
 where s = size (p_types p) (Avar v)
       x = 2 ^ s - 1

readV :: Arg -> String -> IO ()
readV (Aconst c) s = putStrLn $ "movq $" ++ show c ++ ", %" ++ s
readV (Avar v)   s = putStrLn $ "movq -" ++ show (fac*v) ++ "(%rbx), %" ++ s
readOV :: Arg -> String -> IO ()
readOV (Aconst c) s = putStrLn $ "movq $" ++ show c ++ ", %" ++ s
readOV (Avar v)   s = putStrLn $ "movq -" ++ show (fac*v) ++ "(%rax), %" ++ s

writeExp :: Program -> Exp -> IO (Maybe Write)
writeExp _ (Earg a) = readV a "rdi" >> return Nothing
writeExp _ (Ereg v) = do
    putStrLn $ "movq -" ++ show (fac*v) ++ "(%rax), %rdi"
    return Nothing
writeExp _ (Enot a) = readV a "rdi" >> putStrLn "notq %rdi"
                      >> return Nothing
writeExp _ (Ebinop b a1 a2) = do
    readV a1 "rcx"
    readV a2 "rdi"
    case b of
        Or   -> putStrLn "orq %rcx, %rdi"
        Xor  -> putStrLn "xorq %rcx, %rdi"
        And  -> putStrLn "andq %rcx, %rdi"
        Nand -> putStrLn "andq %rcx, %rdi" >> putStrLn "not %rdi"
    return Nothing
writeExp _ (Emux a1 a2 a3) = do
    readV a3 "rcx"
    readV a1 "rdx"
    readV a2 "rdi"
    putStrLn "test %rcx, %rcx"
    putStrLn "cmovqe %rdx, %rdi"
    return Nothing
-- TODO check overflow
writeExp p (Erom _ w r) = do
    readV r "rcx"
    putStrLn $ "movq -" ++ show (fac * 2) ++ "(%rbp), %rdx"
    putStrLn "salq 2, %rcx"
    putStrLn $ "movq 0(%rdx, %rcx, "
            ++ show (w `div` 4) ++ "), %rdi"
    return Nothing
 where n = (+) 1 $ snd $ A.bounds $ p_types p
writeExp _ (Eram _ w ra we wa dt) = do
    readV ra "rcx"
    putStrLn $ "movq (%rbp), %rdx"
    putStrLn "salq 2, %rcx"
    putStrLn $ "movq 0(%rdx, %rcx, "
            ++ show (w `div` 4) ++ "), %rdi"
    return $ Just (w, we, wa, dt)
writeExp p (Econcat a1 a2) = do
    readV a1 "rcx"
    readV a2 "rdi"
    let s = size (p_types p) a2
    putStrLn $ "salq " ++ show s ++ ", %rcx"
    putStrLn "or %rcx, %rdi"
    return Nothing
writeExp _ (Eslice i1 i2 a) = do
    readV a "rdi"
    let x = 2 ^ (i2 - i1 + 1) - 1
    putStrLn $ "sarq " ++ show i1 ++ ", %rdi"
    -- putStrLn $ "andq $" ++ show x ++ ", %rdi"
    return Nothing
writeExp _ (Eselect i a) = do
    readV a "rdi"
    putStrLn $ "sarq " ++ show i ++ ", %rdi"
    -- putStrLn "andq $1, %rdi"
    return Nothing

size :: A.Array Int Type -> Arg -> Int64
size tps (Avar v)   = case (tps ! v) of
    TBit   -> 1
    TNap n -> n
size _   (Aconst c) = ceiling $ logBase 2 $ fromIntegral c

