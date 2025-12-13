module Eval where

import AST

eval :: Expr -> Double
eval (Number n) = n
eval (Boolean b) = if b then 1.0 else 0.0
eval (Unary op expr) = 
    let val = eval expr
    in case op of
        Neg -> -val
        Not -> if val == 0.0 then 1.0 else 0.0
eval (Binary op l r) = 
    let lv = eval l
        rv = eval r
    in case op of
        Add -> lv + rv
        Sub -> lv - rv
        Mul -> lv * rv
        Div -> lv / rv
        Mod -> fromIntegral (round lv `mod` round rv) -- crude hack for double mod
        
        -- Relational (returns 1.0 for true, 0.0 for false)
        Lt -> if lv < rv then 1.0 else 0.0
        Gt -> if lv > rv then 1.0 else 0.0
        Le -> if lv <= rv then 1.0 else 0.0
        Ge -> if lv >= rv then 1.0 else 0.0
        Eq -> if lv == rv then 1.0 else 0.0
        NeQ -> if lv /= rv then 1.0 else 0.0
        
        -- Logical
        And -> if (lv /= 0.0) && (rv /= 0.0) then 1.0 else 0.0
        Or -> if (lv /= 0.0) || (rv /= 0.0) then 1.0 else 0.0
