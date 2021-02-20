{-# Language MagicHash #-}
{-# Language UnboxedSums #-}
{-# Language UnboxedTuples #-}
{-# Language TypeFamilies #-}
{-# Language PolyKinds #-}
{-# Language BangPatterns #-}
{-# Language DataKinds #-}
{-# Language PatternSynonyms #-}
{-# Language RankNTypes #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeApplications #-}
{-# Language RebindableSyntax #-}
{-# Language ImportQualifiedPost #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}

module Def where

import Unlifted.Internal.Class
import Unlifted.Internal.List
import Unlifted.Internal.Maybe
import GHC.Types
import Prelude (otherwise, not, (++), ShowS, (.), showString, showParen,($), (&&), (||))
import Prelude qualified
import System.IO qualified as IO

import Rep

instance EqRep Rep where
  eqDef x y = not (x /= y)
  neDef x y = not (x == y)

instance OrdRep Rep where
  compareDef x y
    | x == y = EQ
    | x <= y = LT
    | otherwise = GT

  ltDef x y = case compare x y of LT -> True;  _ -> False
  leDef x y = case compare x y of GT -> False; _ -> True
  gtDef x y = case compare x y of GT -> True;  _ -> False
  geDef x y = case compare x y of LT -> False; _ -> True

  maxDef x y
    | x <= y = y
    | otherwise = x

  minDef x y
    | x <= y = x
    | otherwise = y

instance NumRep Rep where 
  negateDef a = 0 - a
  minusDef a b = a + negate b

instance FractionalRep Rep where
  fractionalDef x y = x * recip y
  recipDef x = 1 / x

instance RealRep Rep where
  realToFracDef x = fromRational (toRational x)

instance EnumRep Rep where
{-
  enumFromDef x             = map toEnum [fromEnum x ..]
  enumFromThenDef x y       = map toEnum [fromEnum x, fromEnum y ..]
  enumFromToDef x y         = map toEnum [fromEnum x .. fromEnum y]
  enumFromThenToDef x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]
-}
  succDef x = toEnum (fromEnum x + 1)
  predDef x = toEnum (fromEnum x - 1)

instance IntegralRep Rep where
  n `quotDef` d          =  q  where !(# q, _ #) = quotRem n d
  n `remDef` d           =  r  where !(# _, r #) = quotRem n d
  n `divDef` d           =  q  where !(# q, _ #) = divMod n d
  n `modDef` d           =  r  where !(# _, r #) = divMod n d
  divModDef n d
    | signum r == negate (signum d) = (# q - 1, r + d #)
    | otherwise = qr
    where !qr@(# q, r #) = quotRem n d

instance RealFracRep Rep where
  truncateDef x = fromInteger m where
    !(# m, _ #) = properFraction x
  {-# INLINE truncateDef #-}

  roundDef x =
    let !(# n, r #) = properFraction x
        m | r < 0     = n - 1
          | otherwise = n + 1
    in case signum (abs r - 0.5) of
      -1 -> fromInteger n
      0 | Prelude.even n -> fromInteger n
        | otherwise -> fromInteger m
      1 -> fromInteger m
      _ -> Prelude.error "round default defn: Bad value"

  ceilingDef x
    | r > 0 = fromInteger (n + 1)
    | otherwise = fromInteger n
    where !(# n, r #) = properFraction x

  floorDef x 
    | r < 0 = fromInteger (n - 1)
    | otherwise = fromInteger n
    where !(# n, r #) = properFraction x

instance FloatingRep Rep where
  {-# INLINE powDef #-}
  {-# INLINE logBaseDef #-}
  {-# INLINE sqrtDef #-}
  {-# INLINE tanDef #-}
  {-# INLINE tanhDef #-}
  powDef x y = exp (log x * y)
  logBaseDef x y = log y / log x
  sqrtDef x = x ** 0.5
  tanDef x = sin x / cos x
  tanhDef x = sinh x / cosh x

  {-# INLINE log1pDef #-}
  {-# INLINE expm1Def #-}
  {-# INLINE log1pexpDef #-}
  {-# INLINE log1mexpDef #-}
  log1pDef x = log (1 + x)
  expm1Def x = exp x - 1
  log1pexpDef x = log1p (exp x)
  log1mexpDef x = log1p (negate (exp x))

instance RealFloatRep Rep where
  exponentDef x
    | m == 0 = 0
    | otherwise = n + floatDigits x
    where !(# m, n #) = decodeFloat x

  significandDef x = encodeFloat m (negate (floatDigits x))
    where !(# m, _ #) = decodeFloat x

  scaleFloatDef 0 x =  x
  scaleFloatDef k x
    | isFix =  x
    | otherwise =  encodeFloat m (n + clamp b k)
    where
      !(# m, n #) = decodeFloat x
      !(# l, h #) = floatRange x
      d = floatDigits x
      b = h - l + 4 * d
      isFix = x == 0 || isNaN x || isInfinite x
      clamp :: Int -> Int -> Int
      clamp bd k' = max (-bd) (min bd k')

  atan2Def y x
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    |(x <= 0 && y < 0)            ||
     (x <  0 && isNegativeZero y) ||
     (isNegativeZero x && isNegativeZero y)
                       = -atan2 (-y) x
    | y == 0 && (x < 0 || isNegativeZero x)
                       =  pi    -- must be after the previous test on zero y
    | x==0 && y==0     =  y     -- must be after the other double zero tests
    | otherwise        =  x + y -- x or y is a NaN, return a NaN (via +)

data ListDef (a :: TYPE Rep)
  = Nil
  | a :# ListDef a

infixr 5 :#

instance ListRep Rep where
  type List = ListDef
  cons# = (:#) 
  nil# = Nil
  uncons# (a :# as) = Maybe# (# | (# a, as #) #)
  uncons# Nil = Maybe# (# (##) | #)

instance Functor ListDef where
  fmap _ Nil = Nil
  fmap f (a :# as) = f a :# fmap f as

instance Eq a => Prelude.Eq (ListDef a) where
  Nil == Nil = True
  a :# as == b :# bs = a == b && as == bs
  _ == _ = False

  Nil /= Nil = True
  a :# as /= b :# bs = a /= b || as /= bs
  _ /= _ = False

instance Ord a => Prelude.Ord (ListDef a) where
  compare Nil Nil = EQ
  compare Nil (:#){} = LT
  compare (:#){} Nil = GT
  compare (a:#as) (b:#bs) = compare a b <> compare as bs

instance ShowList a => Prelude.Show (ListDef a) where
  showsPrec _ = showList

data MaybeDef (a :: TYPE Rep)
  = Nothing
  | Just a

instance Functor MaybeDef where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Eq a => Prelude.Eq (MaybeDef a) where
  Nothing == Nothing = True
  Just a == Just b = a == b
  _ == _ = False
  
instance Ord a => Prelude.Ord (MaybeDef a) where
  compare Nothing  Nothing = EQ
  compare Nothing  Just{} = LT
  compare Just{}   Nothing = GT
  compare (Just a) (Just b) = compare a b

instance Show a => Prelude.Show (MaybeDef a) where
  showsPrec _ Nothing = showString "Nothing"
  showsPrec d (Just a) = showParen (d >= 11) $ showString "Just " . showsPrec 11 a

instance MaybeRep Rep where
  type Maybe = MaybeDef
  nothing = Nothing
  just = Just
  maybe n _ Nothing = n
  maybe _ j (Just a) = j a

instance MaybeRep# Rep where
  nothing# = Maybe# (# (##) | #)
  just# a = Maybe# (# | a #)
  maybe# n _ (Maybe# (# (##) | #)) = n
  maybe# _ j (Maybe# (# | a #)) = j a

pattern Nothing# :: forall (a :: TYPE Rep). Maybe# a
pattern Nothing# = Maybe# (# (##) | #)

pattern Just# :: forall (a :: TYPE Rep). a -> Maybe# a
pattern Just# a = Maybe# (# | a #)

instance Functor (Maybe# @Rep) where
  fmap _ Nothing# = Nothing#
  fmap f (Just# a) = Just# (f a)

instance Show a => Show (Maybe# (a :: TYPE Rep)) where
  showsPrec _ Nothing# = showString "Nothing#"
  showsPrec d (Just# a) = showParen (d >= 11) $ showString "Just# " . showsPrec 11 a
  show x = shows x ""

-- this instance will probably not fire without a lot of help, because that body condition is harsh
-- We split ShowList into a separate class, even if this breaks compat with base because of this
-- instance
instance (ListRep ('SumRep '[ 'TupleRep '[], Rep ]), Show a) => ShowList (Maybe# (a :: TYPE Rep)) where
  showList = go shows where
    go :: forall (a :: TYPE Rep). (Maybe# a -> ShowS) -> List (Maybe# a) -> ShowS
    go showx l s = case uncons# l of
      Maybe# (# (##) | #) -> "[]" ++ s
      Maybe# (# | (# x, xs #) #) -> '[' : showx x (showl xs)
        where 
          showl l' = case uncons# l' of
            Maybe# (# (##) | #) -> ']' : s
            Maybe# (# | (# y, ys #) #) -> ',' : showx y (showl ys)

{-# complete Nothing#, Just# :: Maybe# #-}

instance ShowRep Rep where
  showsPrecDef _ x s = show x ++ s
  showDef x          = shows x ""

instance ShowListRep Rep where
  showListDef = showList__ shows

showList__ :: forall (a :: TYPE Rep). (a -> ShowS) -> List a -> ShowS
showList__ _     Nil       s = "[]" ++ s
showList__ showx (x :# xs) s = '[' : showx x (showl xs)
  where
    showl Nil       = ']' : s
    showl (y :# ys) = ',' : showx y (showl ys)

instance PrintRep Rep where
  hPrint h x = IO.hPutStrLn h (show x)
