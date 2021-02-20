{-# Language PatternSynonyms #-}
module Common where

pattern Zero :: (Num a, Eq a) => a
pattern Zero = 0

pattern One :: (Num a, Eq a) => a
pattern One = 1

pattern Four :: (Num a, Eq a) => a
pattern Four = 4

clamp :: Int -> Int -> Int
clamp bd k = max (-bd) (min bd k)
