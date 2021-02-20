module Instances where

import Classes
import Rep

instance EqRep Rep where
  eqDefault x y = not (x /= y)
  neDefault x y = not (x == y)

instance OrdRep Rep where
  compareDefault x y
    | x == y = EQ
    | x <= y = LT
    | otherwise = GT

  ltDefault x y = case compare x y of LT -> True;  _ -> False
  leDefault x y = case compare x y of GT -> False; _ -> True
  gtDefault x y = case compare x y of GT -> True;  _ -> False
  geDefault x y = case compare x y of LT -> False; _ -> True

  maxDefault x y
    | x <= y = y
    | otherwise = x

  minDefault x y
    | x <= y = x
    | otherwise = y

instance NumRep Rep where 
  negateDefault a = 0 - a
  minusDefault a b = a + negate b

data instance ListD (a :: TYPE Rep) = Nil# | Cons# a (List# a)

instance ListRep Rep where
  cons# = Cons#
  nil# = Nil#
  uncons# (Cons# a as) = (# | (# a, as #) #)
  uncons# Nil# = (# (##) | #)

data instance MaybeD (a :: TYPE Rep) = Nothing# | Just# a

instance MaybeRep Rep where
  nothing# = Prelude.Nothing
  just# = Prelude.Just
  maybe# _ j (Just a) = j a
  maybe# n _ Nothing = n
  nothing## = Maybe# (# (##) | #)
  just## a = Maybe# (# | a #)
  maybe## _ j (Maybe# (# | a #)) = j a
  maybe## n _ (Maybe# (# (##) | #)) = n

{-
