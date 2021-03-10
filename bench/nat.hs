{-# LANGUAGE MagicHash, UnboxedSums, ImportQualifiedPost, NoImplicitPrelude, RebindableSyntax, UnliftedNewtypes #-}
import Control.Exception
import Criterion.Main
import Criterion.Types
import Criterion.Measurement.Types
import Data.Functor
import Data.Int
import Data.List qualified as List
import GHC.Exts
import GHC.Prim
import Unboxed.Natural
import Unboxed.Prelude as UP
import Prelude (Integer, pure, map, fromIntegral, (>>), (.), IO, seq)
import Whnf

data B = B (# Int# | Integer #)

instance Show B where
  showsPrec d x = showsPrec d (tointeger x)
  show x = show (tointeger x)

add :: B -> B -> B
add (B x) (B y) = case x of
  (# x | #) -> case y of
    (# y | #) -> case addIntC# x y of
      (# result, 0# #) -> B (# result | #)
      _ -> B (# | fromIntegral (I# x) + fromIntegral (I# y) #)
    (# | integer #) -> B (# | fromIntegral (I# x) + integer #)
  (# | integer #) -> case y of
    (# i | #) -> B (# | fromIntegral (I# i) + integer #)
    (# | integer2 #) -> B (# | integer + integer2 #)

tointeger :: B -> Integer
tointeger (B x) = case x of
    (# x | #) -> fromIntegral $ I# x
    (# | y #) -> y

fromint ::Integer -> B
fromint x = if x <= fromIntegral (minBound::Int) || x >= fromIntegral (maxBound::Int)
  then B (# | x #)
  else let I# i = fromInteger x in B (# i | #)

newtype B# = B# (# Int# | Integer #)

instance Show B# where
  showsPrec d x = showsPrec d (tointeger# x)
  show x = show (tointeger# x)

add# :: B# -> B# -> B#
add# (B# x) (B# y) = case x of
  (# x | #) -> case y of
    (# y | #) -> case addIntC# x y of
      (# result, 0# #) -> B# (# result | #)
      _ -> B# (# | fromIntegral (I# x) + fromIntegral (I# y) #)
    (# | integer #) -> B# (# | fromIntegral (I# x) + integer #)
  (# | integer #) -> case y of
    (# i | #) -> B# (# | fromIntegral (I# i) + integer #)
    (# | integer2 #) -> B# (# | integer + integer2 #)

tointeger# :: B# -> Integer
tointeger# (B# x) = case x of
    (# x | #) -> fromIntegral $ I# x
    (# | y #) -> y

fromint# :: Integer -> B#
fromint# x = if x <= fromIntegral (minBound::Int) || x >= fromIntegral (maxBound::Int)
  then B# (# | x #)
  else let I# i = fromInteger x in B# (# i | #)

main :: IO ()
main = do
  defaultMain $ pure $ bgroup "add"
    [ bench "B" (whnfL add'B (fromint (-1000000)))
    , bench "B#" (whnfB# add'B# (fromint# (-1000000)))
    , bench "Int" (whnfL add'Int (-1000000))
    , bench "Integer" (whnfL add'Integer (-1000000))
    ]
  defaultMain $ pure $ bgroup "sub"
    [ bench "Int" (whnfL sub'Int 1000000)
    , bench "Integer" (whnfL sub'Integer 1000000)
    , bench "Natural#" (whnfN# sub'Natural# 1000000)
    ]
  where
    add'B :: B -> ()
    add'B b@(B x) = case x of
      (# 0# | #) -> ()
      _ -> add'B (b `add` fromint 1)

    add'B# :: B# -> ()
    add'B# b@(B# x) = case x of
      (# 0# | #) -> ()
      _ -> add'B# (b `add#` fromint# 1)

    add'Int :: Int -> ()
    add'Int 0 = ()
    add'Int a = add'Int (a + 1)

    add'Integer :: Integer -> ()
    add'Integer 0 = ()
    add'Integer a = add'Integer (a + 1)

    sub'Int :: Int -> ()
    sub'Int 0 = ()
    sub'Int a = sub'Int (a - 1)

    sub'Integer :: Integer -> ()
    sub'Integer 0 = ()
    sub'Integer a = sub'Integer (a - 1)

    sub'Natural# :: Natural# -> ()
    sub'Natural# 0 = ()
    sub'Natural# a = sub'Natural# (a - 1)
