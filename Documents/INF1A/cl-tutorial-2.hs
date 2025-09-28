--Question 1
--B, C, D, E can all be the odd one out, depending on the predicate we choose to focus on
--B because it is the only one that does not have a thick, black border
--C because it is the only one that is a circle
--D because it is the only that's blue
--E because it is the only small shape with a smaller width and height


--Question 2
data Thing = A | B | C | D | E
  deriving (Eq, Show)

things :: [Thing]
things = [A, B, C, D, E]

data Colour = Orange | Blue
  deriving (Eq, Show)

data Shape = Square | Circle
  deriving (Eq, Show)

data Size = Big | Small
  deriving (Eq, Show)

data Border = Thick | Thin
  deriving (Eq, Show)

colour :: Thing -> Colour
colour A = Orange
colour B = Orange
colour C = Orange
colour D = Blue
colour E = Orange

shape :: Thing -> Shape
shape A = Square
shape B = Square
shape C = Circle
shape D = Square
shape E = Square

size :: Thing -> Size
size A = Big
size B = Big
size C = Big
size D = Big
size E = Small

border :: Thing -> Border
border A = Thick
border B = Thin
border C = Thick
border D = Thick
border E = Thick


--Question 3
type Predicate u = u -> Bool

isBig :: Predicate Thing
isBig x = size x == Big
isSmall :: Predicate Thing
isSmall x = size x == Small

isSquare :: Predicate Thing
isSquare x = shape x == Square
isCircle :: Predicate Thing
isCircle x = shape x == Circle

isOrange :: Predicate Thing
isOrange x = colour x == Orange
isBlue :: Predicate Thing
isBlue x = colour x == Blue

thickBorder :: Predicate Thing
thickBorder x = border x == Thick
thinBorder :: Predicate Thing
thinBorder x = border x == Thin


{-
Question 4
1. Every blue square has a thin border. (False)
[ (x, thinBorder x) | x <- things, isBlue x && isSquare x ]
--[(D,False)]

2. Some amber circle is not big. (False)
[ (x, isSmall x) | <- things, isOrange x && isCircle x]
--[(C,False)]


Question 5
Original: No square is blue -> False

1. It is not the case that there is a square that is blue
not (or [isBlue x | x <- things, isSquare x ])
--False

2. Every square is not blue
and [isOrange x | x <- things, isSquare x]
--False
-}


--Question 6
--List all the remaining 4 things that are different from x
thingsOtherThan :: Thing -> [Thing]
thingsOtherThan x = [diff | diff <- things, diff /= x]

properties :: [Predicate Thing]
properties = [isBlue, isOrange, isCircle, isSquare, isBig, isSmall, thickBorder, thinBorder]

--Convert function name to string as ghci can't print function names...
convertToString :: [(String,Predicate Thing)]
convertToString = [("isOrange", isOrange), ("isBlue", isBlue), ("isSquare", isSquare), ("isCircle", isCircle), ("isBig", isBig), ("isSmall", isSmall), ("thickBorder", thickBorder), ("thinBorder", thinBorder)]

--List all the properties of x
propertiesOf :: Thing -> [String]
propertiesOf x = [name | (name, p) <- convertToString, p x]

isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing p x = or [p xs | xs <- thingsOtherThan x]

propertiesOnlyOf :: Thing -> [String]
propertiesOnlyOf x = [name | (name,p) <- convertToString, not (isPropertyOfAnotherThing p x)]

rank :: Thing -> Int
rank x = length (propertiesOnlyOf x)

--A stands out as it is the only one with a rank of 1!