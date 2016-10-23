
module Main where
import Parser
import Compil
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then exitFailure else return ()
    nt <- parseNET $ head args
    compile nt
    exitSuccess

