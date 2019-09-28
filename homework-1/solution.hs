module HW1 where


-- | A representation of arithmetic expressions as binary trees where leaves
--   are literal integers and internal nodes are either addition or
--   multiplaction nodes. Note that this data structure is an "abstract
--   syntax tree", which is something we will spend more time talking about
--   later in the course.
data Expr
   = Lit Int         -- ^ Literal integers
   | Add Expr Expr   -- ^ Addition expressions
   | Mul Expr Expr   -- ^ Multiplication expressions
  deriving (Eq,Show)


-- | The expression: 2 + 3 * 4
e1 :: Expr
e1 = Add (Lit 2) (Mul (Lit 3) (Lit 4))


-- | The expression: (7 + 6) * 5
e2 :: Expr
e2 = Mul (Add (Lit 7) (Lit 6)) (Lit 5)


-- | The expresssion: 3 * 2 + 5 * 4
e3 :: Expr
e3 = Add (Mul (Lit 3) (Lit 2)) (Mul (Lit 5) (Lit 4))


-- | The expression: 8 + 7 * 9 + 6
e4 :: Expr
e4 = Add (Lit 8) (Add (Mul (Lit 7) (Lit 9)) (Lit 6))


-- | The leftmost literal in an expression.
--
--   >>> leftLit (Lit 3)
--   3
--
--   >>> leftLit e1
--   2
--   
--   >>> leftLit e2
--   7
--
leftLit :: Expr -> Int
leftLit (Lit l) = l
leftLit (Add e _) = leftLit e
leftLit (Mul e _) = leftLit e


-- | The rightmost literal in an expression.
--
--   >>> rightLit (Lit 3)
--   3
--
--   >>> rightLit e3
--   4
--   
--   >>> rightLit e4
--   6
--
rightLit :: Expr -> Int
rightLit (Lit l) = l
rightLit (Add _ e) = rightLit e
rightLit (Mul _ e) = rightLit e


-- | Get the maximum literal value in an expression.
--
--   >>> maxLit (Lit 3)
--   3
--
--   >>> maxLit e1
--   4
--
--   >>> maxLit e2
--   7
--
--   >>> maxLit e3
--   5
--
--   >>> maxLit e4
--   9
--
maxLit :: Expr -> Int
maxLit (Lit l) = l
maxLit (Add el er) = maxLitBetween el er
maxLit (Mul el er) = maxLitBetween el er


-- | Helper logic for computing the max between two expressions. This allows us to efficiently use pattern matching
--   in our `maxLit` function without the need to duplicate logic.
--
--   >>> maxLitBetween (Lit 3) (Lit 5)
--   5
--
--   >>> maxLitBetween (Add (Lit 3) (Lit 4)) (Mul (Lit 5) (Lit 6))
--   6
--
maxLitBetween :: Expr -> Expr -> Int
maxLitBetween el er = (let x = maxLit el in (let y = maxLit er in (if x > y then x else y)))


-- | The integer result of evaluating an expression.
--
--   >>> eval (Lit 3)
--   3
--
--   >>> eval e1
--   14
--
--   >>> eval e2
--   65
--
--   >>> eval e3
--   26
--
--   >>> eval e4
--   77
--
eval :: Expr -> Int
eval (Lit l) = l
eval (Add el er) = (eval el) + (eval er)
eval (Mul el er) = (eval el) * (eval er)