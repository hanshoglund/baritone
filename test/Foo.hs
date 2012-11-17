
{-# LANGUAGE Cpp #-}

module Foo.Bar.Baz where

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


fix      = __fix
mkPair   = __c2
fst      = __g1
snd      = __g2
putStrLn = __trace

ap f x        = f x
id x          = x
const x y     = x
const2 x y z  = x
comp f g x = f (g x)

pred x      = x - 1
succ x      = x + 1
adder a b x = (a*x) + (b*x)







--- Tests
add23 = adder 2 3
pair = (88,99)

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

-- xs = 1 `cons` (2 `cons` (3 `cons` (1 `cons` (2 `cons` (3 `cons` (1 `cons` (2 `cons` (3 `cons` nil))))))))
xs = [1,2,3,4,5,6,7,8,9,10]
main5 = putStrLn (map succ xs)


-- -- Data.Maybe
mkNothing dummy = \n j -> n
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




