module Eval where

import AST
import qualified Data.Map as Map

type Env = Map.Map String Double

eval :: Expr -> Env -> (Double, Env)
eval (Number n) env = (n, env)
eval (Boolean b) env = (if b then 1.0 else 0.0, env)
eval (Identifier name) env = 
    case Map.lookup name env of
        Just val -> (val, env)
        Nothing -> error $ "Undefined variable: " ++ name
eval (Assign name expr) env = 
    let (val, env') = eval expr env
    in (val, Map.insert name val env')
eval (Unary op expr) env = 
    let (val, env') = eval expr env
    in case op of
        Neg -> (-val, env')
        Not -> (if val == 0.0 then 1.0 else 0.0, env')
eval (Binary op l r) env = 
    let (lv, env') = eval l env
        (rv, env'') = eval r env'
    in case op of
        Add -> (lv + rv, env'')
        Sub -> (lv - rv, env'')
        Mul -> (lv * rv, env'')
        Div -> (lv / rv, env'')
        Mod -> (fromIntegral (round lv `mod` round rv), env'')
        
        -- Relational
        Lt -> (if lv < rv then 1.0 else 0.0, env'')
        Gt -> (if lv > rv then 1.0 else 0.0, env'')
        Le -> (if lv <= rv then 1.0 else 0.0, env'')
        Ge -> (if lv >= rv then 1.0 else 0.0, env'')
        Eq -> (if lv == rv then 1.0 else 0.0, env'')
        NeQ -> (if lv /= rv then 1.0 else 0.0, env'')
        
        -- Logical
        And -> (if (lv /= 0.0) && (rv /= 0.0) then 1.0 else 0.0, env'')
        Or -> (if (lv /= 0.0) || (rv /= 0.0) then 1.0 else 0.0, env'')
