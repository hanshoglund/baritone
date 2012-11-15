
module Foo where

one = 1
name = "hans"
char = 'c'

-- id x = x -- FIXME

id = \x -> x
const = \x -> \y -> x
app  = \f x -> f x
app2 = \f x y -> f x y

foo = \x y -> x + (succ $ y)
bar = 1 + (2 * 3)

-- foo = \x -> 1
-- bar = \x y -> x

