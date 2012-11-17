
{-# LANGUAGE Cpp #-}

module Foo.Bar.Baz where

-- #ifdef __GLASGOW_HASKELL__
-- #else
-- A comment
-- | This is one.
__trace a = inline ["trace($)", a]
__neg a   = inline ["-$", a]
__add a b = inline ["$ + $", a, b]
__mul a b = inline ["$ * $", a, b]
__sub a b = inline ["$ - $", a, b]
__div a b = inline ["$ / $", a, b]

__foreach f as = inline ["for each v in $ { $.A(v) }", as, f] -- FIXME
__c2 a b   = inline ["CreateSparseArray($,$)", a, b]
__g1 a     = inline ["$[0]", a]
__g2 a     = inline ["$[1]", a]

consPair = __c2
fst      = __g1
snd      = __g2
putStrLn = __trace

fix f = (\x -> f (\v -> x x v)) (\x -> f (\v -> x x v))

-- #endif


-- -- data List a = Nil | Cons a (List a)
-- #nil      :: List a
-- #cons     :: a -> List a -> List a
-- #caseList :: c -> (a -> List a -> c) -> List a -> c
-- #nil      = \f g -> f
-- #cons x y = \f g -> g x y
-- #caseList f g x = x f g


-- 
one = 1
two = 2
name = "hans"
char = 'c'       
negOne = (-1)

ap f x        = f x
id x          = x
const x y     = x
const2 x y z  = x

comp f g x = f (g x)

pred x = x - 1
succ x = x + 1
adder a b x = (a*x) + (b*x)
add23 = adder 2 3
pair = 555 : 666

main  = putStrLn `ap` (add23 10)
main1 = putStrLn `ap` ((comp succ succ) 0)
main2 = putStrLn `ap` (fst pair)
main3 = putStrLn `ap` (snd pair)
main4 = putStrLn `ap` "This is amazing!"


-- -- Data.List
mkNil dummy     = \f g -> f
nil             = \f g -> f
cons x xs       = \f g -> g x xs

map = fix (\map_ f xs -> xs (mkNil 0) (\x xs -> (f x `cons` map_ f xs)))

xs = 1 `cons` (2 `cons` (3 `cons` (1 `cons` (2 `cons` (3 `cons` (1 `cons` (2 `cons` (3 `cons` nil))))))))
main5 = putStrLn (map succ xs)


-- -- Data.Maybe
mkNothing dummy = \n j -> n -- workaround
nothing = \n j -> n
just x  = \n j -> j x

mapMaybe f v = v (mkNothing 0) (just `comp` f)
main6 = putStrLn (mapMaybe succ nothing)
main7 = putStrLn (mapMaybe succ (just 220))





    
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




