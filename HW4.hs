module HW4 where

import Prelude hiding (Enum (..), sum)

--
-- * Natural numbers
--

-- | The natural numbers.
data Nat
    = Zero
    | Succ Nat
    deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--
--   >>> pred Zero
--   Zero
--
--   >>> pred three
--   Succ (Succ Zero)
--
pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ x) = x


-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False


-- | Convert a natural number to an integer. NOTE: We use this function in
--   tests, but you should not use it in your other definitions!
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ x) = 1 + toInt x


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--
add :: Nat -> Nat -> Nat
add Zero Zero         = Zero
add Zero (Succ x)     = Succ(add Zero x)
add (Succ x) Zero     = Succ(add Zero x)
add (Succ x) (Succ y) = (Succ(Succ(add x y)))


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub Zero Zero         = Zero
sub (Succ x) Zero     = Succ x
sub Zero (Succ x)     = Zero
sub (Succ x) (Succ y) = sub x y


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt (Succ x) Zero     = True
gt Zero (Succ y)     = False
gt Zero Zero         = False
gt (Succ x) (Succ y) = gt x y


-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three two)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult Zero _               = Zero
mult _ Zero               = Zero
mult (Succ x) (Succ Zero) = (Succ x)
mult (Succ x) (Succ y)    = add (Succ x) (mult (Succ x) y)


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum = foldr add Zero


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat]
odds = godds Zero
    where
        godds :: Nat -> [Nat]
        godds x = (Succ x) : godds (Succ (Succ x))
