module Main where

import System.Environment (getArgs)
import ParseMicroc

main :: IO ()
main =  do
    args <- getArgs
    if length args == 0
        then putStrLn "./main fileName"
        else microcCompilerFromFile $ head args
     --microcCompiler