module Main where
import Grammar
import Tokens
import qualified Data.Map as Map
import Translate



main :: IO ()
main = do
    s <- getContents
    let ast = parseCalc (scanTokens s)
    --print ast
    -- remember the difference between print and putStr
    putStr (translateProgram ast)
    
