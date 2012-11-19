
{-# LANGUAGE Cpp #-}

module Foo where

__false                 = inline ["false"]
__true                  = inline ["true"]
__trace a               = inline ["trace($)", a]
__asc a                 = inline ["Asc($)", a]
__chr a                 = inline ["Chr($)", a]
__charAt n s            = inline ["CharAt($,$)", s, n]
__isObject a            = inline ["IsObject($)", a]
__isValid a             = inline ["IsValid($)", a]
__joinStrings p xs      = inline ["JoinStrings($,$)", xs, p]
__splitString p s       = inline ["SplitString($,$)", s, p, __true]
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

-- Data.Bool

data Bool = True | False

-- Data.List

data List a = Nil | Cons a (List a)
map = fix (\map f xs -> list Nil (\x xs -> (f x `Cons` map f xs)) xs)

-- Data.Maybe

data Maybe a = Nothing | Just a
mkNothing d = \n j -> n -- Workaround of #5

mapMaybe f = maybe (mkNothing 0) (Just . f)


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

xs = [1,2,3,4,5,6,7,8,9,10]
main5 = putStrLn (map succ xs)

main6 = putStrLn (mapMaybe succ Nothing)
main7 = putStrLn (mapMaybe succ (Just 220))


    
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




