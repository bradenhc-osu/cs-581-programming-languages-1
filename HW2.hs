module HW2 where

import HW1


-- | A string rendering of the expression in Reverse Polish Notation (RPN).
--
--   >>> toRPN (Lit 3)
--   "3"
--
--   >>> toRPN e1
--   "2 3 4 * +"
--
--   >>> toRPN e2
--   "7 6 + 5 *"
--
--   >>> toRPN e3
--   "3 2 * 5 4 * +"
--
--   >>> elem (toRPN e4) ["8 7 9 * + 6 +", "8 7 9 * 6 + +"]
--   True
--   
toRPN :: Expr -> String
toRPN (Lit l)   = show l
toRPN (Add l r) = (toRPN l) ++ " " ++ (toRPN r) ++ " +"
toRPN (Mul l r) = (toRPN l) ++ " " ++ (toRPN r) ++ " *"  


-- | Convert a string rendering of an expression in RPN into an expression
--   represented as an abstract syntax tree. You can assume that your function
--   will only be given valid strings, i.e. it need not fail gracefully if it
--   encounters an error.
--
--   >>> fromRPN "3"
--   Lit 3
--
--   >>> fromRPN "2 3 +"
--   Add (Lit 2) (Lit 3)
--
--   >>> fromRPN "2 3 4 + +"
--   Add (Lit 2) (Add (Lit 3) (Lit 4))
--
--   >>> all (\e -> e == fromRPN (toRPN e)) [e1,e2,e3,e4]
--   True
--
-- fromRPN :: String -> Expr
-- fromRPN s = fromRPNHelper [] s

-- fromRPNHelper :: [Expr] -> String -> Expr
-- fromRPNHelper (e:es)   ""      = e
-- fromRPNHelper es       (' ':s) = fromRPNHelper es s
-- fromRPNHelper (x:y:es) ('+':s) = fromRPNHelper ((Add y x):es) s
-- fromRPNHelper (x:y:es) ('*':s) = fromRPNHelper ((Mul y x):es) s
-- fromRPNHelper es       (x:s)   = fromRPNHelper ((Lit (read (x:""))):es) s
fromRPN :: String -> Expr
fromRPN s = head (go [] (words s))
    where
        go :: [Expr] -> [String] -> [Expr]
        go es       []       = es
        go (x:y:es) ("+":ss) = go (Add y x : es) ss
        go (x:y:es) ("*":ss) = go (Mul y x : es) ss
        go es       (x:ss)   = go ((Lit (read s)) : es) ss