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

translateRelExp (ExpC e) = translateExp e
translateRelExp (And a b) = (translateRelExp a) ++ " && " ++ (translateRelExp b)
translateRelExp (Or a b) = (translateRelExp a) ++ " || " ++ (translateRelExp b)
translateRelExp (Not a) = "!" ++ (translateRelExp a)

translateStatement (AssignC varname exp) t = ( (replicate t '\t') ++ varname ++ " = " ++ (translateExp exp)++";\n")
translateStatement (IfC condition stlist) t = ( (replicate t '\t') ++ "if(" ++ (translateRelExp condition) ++ "){\n" ++
                                                (translateStatements stlist (t+1)) ++
                                                (replicate t '\t')++"}\n"
                                              )
translateStatement (IfElC condition ifstlist elstlist) t = ( (replicate t '\t') ++ "if(" ++ (translateRelExp condition) ++ "){\n" ++
                                                (translateStatements ifstlist (t+1)) ++
                                                (replicate t '\t') ++ "else{\n" ++
                                                (translateStatements elstlist (t+1)) ++
                                                (replicate t '\t')++"}\n"
                                              )
translateStatement (WhileC condition stlist ) t = ( (replicate t '\t') ++ "while(" ++ (translateRelExp condition) ++ "){\n" ++ 
                                                    (translateStatements stlist (t+1)) ++
                                                    (replicate t '\t') ++ "}\n" )


translateStatements (x:xs) t = ( (translateStatement x t) ++ (translateStatements xs t) )
translateStatements [] t     = ""

translateArgs (x:xs) = x++" "++(translateArgs xs)
translateArgs []      = ""

translateProgram (Func name args p)  = "function "++name++"("++(translateArgs args)++")"++" {\n"++(translateStatements p 1)++"}\n"


-- some additional code that would be useful
--getEnvFromVarl  varl = foldr (\x map -> case x of (VDef s exp) -> Map.insert s exp map)  varl

--translateExp (Var s)        = case Map.lookup s  of
--                           Just e -> (translateExp e )
--                           Nothing -> error ("variable "++s++" not found")
    
--where ' = Map.insert s e1 


