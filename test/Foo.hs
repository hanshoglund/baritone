
module Foo where

-- A comment
-- | This is one.
one = 1
two = 2
name = "hans"
char = 'c'

id x      = x
const x   = \y -> x
app x y z = x z (y z) 
nest      = \x -> \y -> \z -> x + y + z

foo       = \x -> x + a + b + c