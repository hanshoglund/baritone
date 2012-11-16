
module Foo where

-- A comment
-- | This is one.
one = 1
two = 2
name = "hans"
char = 'c'
add_    = 0
zipWith = 0
if_     = 0

id x     = x
const x  = \y -> x
const2 x = \y -> \z -> x

adder a b = \x -> (a*x) + (b*x)
fibs = 0 `cons` 1 `cons` zipWith plus fibs (tail fibs)

map f xs = if_ (isNull xs) null (cons (f `ap` head xs) (map f `ap` tail xs))
