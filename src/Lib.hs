module Lib
    ( runParse
    ) where
import System.IO
import Control.Monad
import ParseMicroc


inputLoop::IO()
inputLoop = do
    s <- getLine
    eof  <- isEOF
    when (s /= "") $ putStrLn $ init $ run s
    unless eof inputLoop

runParse :: IO ()
runParse = do
        inputLoop