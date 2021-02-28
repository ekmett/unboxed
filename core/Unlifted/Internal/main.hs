{-# Language TypeFamilies #-}
main :: x ~ IO () => x
main = print "hello"
