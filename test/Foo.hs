
module Foo.Bar.Baz where

-- A comment
-- | This is one.
__trace a = inline ["trace(x)", a]
__neg a   = inline ["-x", a]
__add a b = inline ["x + x", a, b]
__mul a b = inline ["x * x", a, b]
__sub a b = inline ["x - x", a, b]
__div a b = inline ["x / x", a, b]

__foreach f as = inline ["for each v in x { x.A(v) }", as, f]
__c2 a b   = inline ["CreateSparseArray(x,x)", a, b]
__g1 a     = inline ["x[0]", a]
__g2 a     = inline ["x[1]", a]





one = 1
two = 2
name = "hans"
char = 'c'       
-- negOne = (-1)

id x          = x
const x y     = x
const2 x y z  = x

comp f g x = f (g x)

pred x = x - 1
succ x = x + 1
adder a b x = (a*x) + (b*x)
add23 = adder 2 3
pair = __c2 555 666

main  = __trace (add23 10)
main1 = __trace ((comp succ succ) 0)
main2 = __trace (__g1 pair)
main3 = __trace (__g2 pair)


-- f = (a b c)  (\x -> a b x (c d x)) (\y -> \z -> m n y z)
-- f           = (((a b) c) (\x -> ((a b) x) ((c d) x))) (\y -> \z -> ((m n) y) z)

-- f x y z = z
-- g x = \y z -> z
-- h = \x -> \y -> \z -> z

-- fibs = 0 `cons` 1 `cons` zipWith plus fibs (tail fibs)

    
-- data Score
-- data Selection
-- data Staff
-- data SystemStaff
-- data Bar
-- data Barline
-- data Tuplet
-- 
-- data BarRest
-- data Clef
-- data TimeSignature
-- data KeySignature
-- data RehearsalMark
-- data InstrumentChange
-- data SymbolItem
-- data NoteRest
-- data Line
-- data Text     
-- data LyricItem
-- data Comment




