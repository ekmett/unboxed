{-# Language MagicHash #-}
{-# Language UnboxedSums #-}
{-# Language UnboxedTuples #-}
{-# Language TypeFamilies #-}
{-# Language PolyKinds #-}
{-# Language DataKinds #-}
{-# Language PatternSynonyms #-}
{-# Language RankNTypes #-}
{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language ImportQualifiedPost #-}
{-# Language TypeSynonymInstances #-}

module Def where

import Unlifted.Internal.Class
import Unlifted.Internal.List
import Unlifted.Internal.Maybe
import GHC.Types
import Prelude (otherwise, not, (++), ShowS)
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

data MaybeDef (a :: TYPE Rep)
  = Nothing
  | Just a

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

{-# complete Nothing#, Just# :: Maybe# #-}

instance ShowRep Rep where
  showsPrecDef _ x s = show x ++ s
  showDef x          = shows x ""
  showListDef ls   s = showList__ shows ls s

showList__ :: forall (a :: TYPE Rep). (a -> ShowS) -> List a -> ShowS
showList__ _     Nil       s = "[]" ++ s
showList__ showx (x :# xs) s = '[' : showx x (showl xs)
  where
    showl Nil       = ']' : s
    showl (y :# ys) = ',' : showx y (showl ys)

instance PrintRep Rep where
  hPrint h x = IO.hPutStrLn h (show x)
