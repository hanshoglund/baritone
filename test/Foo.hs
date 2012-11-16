
module Foo.Bar.Baz where

-- A comment
-- | This is one.
__neg a     = inline "-%" x
__add a b   = inline "% + %" a b
__mul a b   = inline "% * %" a b

one = 1
two = 2
name = "hans"
char = 'c'       
negOne = (-1)

id x          = x
const x y     = x
const2 x y z  = x

comp1 f g x = f (g x)

adder a b x = (a*x) + (b*x)
add23 = adder 2 3

main = trace (add23 5)



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




