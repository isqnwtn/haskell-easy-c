module Main where
import Compiler.Grammar
import Compiler.Tokens
import Translate

import System.Environment
import System.IO
import Control.Monad



main :: IO ()
main = do
    let readContent = do
        args <- getArgs
        if ( length args ) == 0
            then do
                contents <- getContents
                return contents
            else do
                handle <- openFile (head args) ReadMode
                contents <- hGetContents handle
                return contents

    s <- readContent
    let ast = parseCalc (scanTokens s)
    --print ast
    -- remember the difference between print and putStr
    putStr (translateProgram ast)
    
