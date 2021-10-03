module Translate where
import Compiler.Grammar
import Compiler.Tokens
import qualified Data.Map as Map

translateExp (Int v)         = show v
translateExp (Brack e)      = "(" ++ (translateExp e ) ++ ")"
translateExp (Plus e1 e2)   = (translateExp e1 ) ++ "+" ++ (translateExp e2 )
translateExp (Minus e1 e2)  = (translateExp e1 ) ++ "-" ++ (translateExp e2 )
translateExp (Times e1 e2)  = (translateExp e1 ) ++ "*" ++ (translateExp e2 )
translateExp (Div e1 e2)    = (translateExp e1 ) ++ "/" ++ (translateExp e2 )
translateExp (Negate e)     = "-" ++ (translateExp e )
translateExp (Var s)        = s

translateStatements (x:xs) t =( (replicate t '\t')++ (translateExp x) ++ "\n") ++ (translateStatements xs t)
translateStatements [] t = ""

translateArgs (x:xs) = x++" "++(translateArgs xs)
translateArgs []      = ""

translateProgram (Func name args p)  = "function "++name++"("++(translateArgs args)++")"++" {\n"++(translateStatements p 1)++"}\n"


-- some additional code that would be useful
--getEnvFromVarl  varl = foldr (\x map -> case x of (VDef s exp) -> Map.insert s exp map)  varl

--translateExp (Var s)        = case Map.lookup s  of
--                           Just e -> (translateExp e )
--                           Nothing -> error ("variable "++s++" not found")
    
--where ' = Map.insert s e1 


