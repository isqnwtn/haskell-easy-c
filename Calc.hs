module Main where
import Grammar
import Tokens
import qualified Data.Map as Map

--getEnvFromVarl env varl = foldr (\x map -> case x of (VDef s exp) -> Map.insert s exp map) env varl

translateExp (Int v) _         = show v
translateExp (Brack e) env     = "(" ++ (translateExp e env) ++ ")"
translateExp (Plus e1 e2) env  = (translateExp e1 env) ++ "+" ++ (translateExp e2 env)
translateExp (Minus e1 e2) env = (translateExp e1 env) ++ "-" ++ (translateExp e2 env)
translateExp (Times e1 e2) env = (translateExp e1 env) ++ "*" ++ (translateExp e2 env)
translateExp (Div e1 e2) env   = (translateExp e1 env) ++ "/" ++ (translateExp e2 env)
translateExp (Negate e) env    = "-" ++ (translateExp e env)
<<<<<<< HEAD
translateExp (Var s) env       = case Map.lookup s env of
                           Just e -> (translateExp e env)
                           Nothing -> error ("variable "++s++" not found")
translateExp (Let varl e2) env = translateExp e2 env'
    where env' = getEnvFromVarl env varl
=======
translateExp (Var s) env       = s

--translateExp (Var s) env       = case Map.lookup s env of
--                           Just e -> (translateExp e env)
--                           Nothing -> error ("variable "++s++" not found")
>>>>>>> Converted to translation language
    
--where env' = Map.insert s e1 env


run :: Exp -> String
run e = translateExp e Map.empty

main :: IO ()
main = do
    s <- getContents
    let ast = parseCalc (scanTokens s)
    --print ast
    print (run ast)
    
