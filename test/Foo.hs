
{-# LANGUAGE Cpp #-}

module Foo where

__trace a               = inline ["trace($)", a]
__asc a                 = inline ["Asc($)", a]
__chr a                 = inline ["Chr($)", a]
__charAt n s            = inline ["CharAt($,$)", s, n]
__isObject a            = inline ["IsObject($)", a]
__isValid a             = inline ["IsValid($)", a]
__joinStrings p xs      = inline ["JoinStrings($,$)", xs, p]
-- __splitString p s       = inline ["SplitString($,$)", s, p, True] -- TODO
__lenght a              = inline ["Length($)", a]
__round a               = inline ["Round($)", a]
__floor a               = inline ["RoundUp($)", a]
__ceil a                = inline ["RoundDown($)", a]
__error m               = inline ["StopPlugin($)", m]
__substring m n s       = inline ["Substring($)", s, m, n]

__getValidationError a   = inline ["GetValidationError($)", a]
__validationChecking s a = inline ["ValidationChecking($,$)", s, a]

__neg a   = inline ["-$", a]
__add a b = inline ["$ + $", a, b]
__mul a b = inline ["$ * $", a, b]
__sub a b = inline ["$ - $", a, b]
__div a b = inline ["$ / $", a, b]
__mod a b = inline ["$ % $", a, b]

__not a   = inline ["not $", a]
__and a b = inline ["$ and $", a, b]
__or a b  = inline ["$ or $", a, b]

__eq a b  = inline ["$ = $", a, b]
__ne a b  = inline ["$ != $", a, b]
__lt a b  = inline ["$ < $", a, b]
__gt a b  = inline ["$ > $", a, b]
__le a b  = inline ["$ <= $", a, b]
__ge a b  = inline ["$ >= $", a, b]

__c2 a b  = inline ["CreateSparseArray($,$)", a, b]
__g1 a    = inline ["$[0]", a]
__g2 a    = inline ["$[1]", a]

__fix f = (\x -> f (\v -> x x v)) (\x -> f (\v -> x x v))

__nil        = \f g -> f
__cons x xs  = \f g -> g x xs

--------------------------------------------------------------------------------
-- Prelude
--------------------------------------------------------------------------------

(+)  = __add
(-)  = __sub
(*)  = __mul
(/)  = __div
mod  = __mod
(<)  = __lt
(<=) = __le
(>)  = __gt
(>=) = __ge
(==) = __eq
(/=) = __ne

error = __error

fix      = __fix
fst      = __g1
snd      = __g2
putStrLn = __trace

f $ x         = f x
id x          = x
const x y     = x
const2 x y z  = x
f . g         = \x -> f (g x)

pred x      = x - 1
succ x      = x + 1



--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------


adder a b x = (a*x) + (b*x)
add23 = adder 2 3
main  = putStrLn $ add23 10

pair = (88,99)
main1 = putStrLn $ (succ . succ) 0
main2 = putStrLn $ fst pair
main3 = putStrLn $ snd pair

main4 = putStrLn $ "This is amazing!"


-- Data.List

-- data List a = Nil | Cons a (List a)

mkNil d   = \f g -> f
cons x xs = \f g -> g x xs

map = fix (\map_ f xs -> xs (mkNil 0) (\x xs -> (f x : map_ f xs)))

xs = [1,2,3,4,5,6,7,8,9,10]
main5 = putStrLn (map succ xs)



-- Data.Maybe

-- data Maybe a = Nothing | Just a
mkNothing d = \n j -> n
just x      = \n j -> j x

mapMaybe f v = v (mkNothing 0) (just . f)
main6 = putStrLn (mapMaybe succ (mkNothing 0))
main7 = putStrLn (mapMaybe succ (just 220))



main11 = putStrLn $ id id 2



-- foo f (Bar x y) = a x y
-- foo f (Baz x y) = b x y
-- 
-- foo _1 _2 = case (_1,_2) of
--     f _3 -> _3 (\x y -> a x y)
--     f _4 -> _4 (\x y -> b x y)
    




    
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




