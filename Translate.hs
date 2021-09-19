module Translate where
import Grammar
import Tokens
translateExp (Int v) _         = show v
translateExp (Brack e) env     = "(" ++ (translateExp e env) ++ ")"
translateExp (Plus e1 e2) env  = (translateExp e1 env) ++ "+" ++ (translateExp e2 env)
translateExp (Minus e1 e2) env = (translateExp e1 env) ++ "-" ++ (translateExp e2 env)
translateExp (Times e1 e2) env = (translateExp e1 env) ++ "*" ++ (translateExp e2 env)
translateExp (Div e1 e2) env   = (translateExp e1 env) ++ "/" ++ (translateExp e2 env)
translateExp (Negate e) env    = "-" ++ (translateExp e env)
translateExp (Var s) env       = s


-- some additional code that would be useful
--getEnvFromVarl env varl = foldr (\x map -> case x of (VDef s exp) -> Map.insert s exp map) env varl

--translateExp (Var s) env       = case Map.lookup s env of
--                           Just e -> (translateExp e env)
--                           Nothing -> error ("variable "++s++" not found")
    
--where env' = Map.insert s e1 env


