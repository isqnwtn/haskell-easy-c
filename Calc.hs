module Main where
import Grammar
import Tokens
import qualified Data.Map as Map
import Translate


run :: Exp -> String
run e = translateExp e Map.empty

main :: IO ()
main = do
    s <- getContents
    let ast = parseCalc (scanTokens s)
    --print ast
    print (run ast)
    
