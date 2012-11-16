
module Foo where

-- A comment
-- | This is one.
one = 1
two = 2
name = "hans"
char = 'c'       
negOne = (-1)

add_    = 0
zipWith = 0
if_     = 0
cons    = 0
ap      = 0
head    = 0
tail    = 0
map     = 0

id x     = x
const x  = \y -> x
const2 x = \y -> \z -> x
comp f g = \x -> f (g x)

-- adder a b = \x -> (a*x) + (b*x)
fibs = 0 `cons` 1 `cons` zipWith plus fibs (tail fibs)

map f xs = if_ (isNull xs) null (cons (f `ap` head xs) (map f `ap` tail xs))
    


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
