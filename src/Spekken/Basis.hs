{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Spekken.Basis where


-- Z basis:
-- Zero     ~ |0⟩ = ■■□□
-- One      ~ |1⟩ = □□■■

-- X basis:
-- Positive ~ |+⟩ = ■□■□
-- Negative ~ |-⟩ = □■□■

-- Y basis:
-- Up       ~ |↑⟩ = ■□□■
-- Down     ~ |↓⟩ = □■■□

-- 1 ~ ■■■■ (constant truth, mixed, full ignorance)
-- 0 ~ □□□□ (constant falsity)

data Z = Zero | One 
    deriving (Eq, Show)
data X = Positive | Negative 
    deriving (Eq, Show)
data Y = Up | Down
    deriving (Eq, Show)

type ZX = Either Z X 
type ZY = Either Z Y
type XY = Either X Y
type XZ = Either X Z
type YZ = Either Y Z
type YX = Either Y X


class (Enum a) => Basis a where
    falseB   :: a
    trueB    :: a
    notB     :: a -> a
    sqrtNotB :: a -> a
    basis    :: [a]
    basis     = [falseB, trueB]
    

instance Basis Bool where
    falseB   = False
    trueB    = True
    notB     = not
    sqrtNotB = error "undefined."


instance Enum ZX where
    toEnum 0 = Left Zero
    toEnum 1 = Right Positive
    toEnum 2 = Left One
    toEnum 3 = Right Negative
    toEnum n = toEnum (n `mod` 4)
    fromEnum (Left Zero) = 0
    fromEnum (Right Positive) = 1
    fromEnum (Left One) = 2
    fromEnum (Right Negative) = 3

instance Enum ZY where
    toEnum 0 = Left Zero
    toEnum 1 = Right Up
    toEnum 2 = Left One
    toEnum 3 = Right Down
    toEnum n = toEnum (n `mod` 4)
    fromEnum (Left Zero) = 0
    fromEnum (Right Up) = 1
    fromEnum (Left One) = 2
    fromEnum (Right Down) = 3

instance Enum XY where
    toEnum 0 = Left Positive
    toEnum 1 = Right Up
    toEnum 2 = Left Negative
    toEnum 3 = Right Down
    toEnum n = toEnum (n `mod` 4)
    fromEnum (Left Positive) = 0
    fromEnum (Right Up) = 1
    fromEnum (Left Negative) = 3
    fromEnum (Right Down) = 4

instance Enum XZ where
    toEnum 0 = Left Positive
    toEnum 1 = Right Zero
    toEnum 2 = Left Negative
    toEnum 3 = Right One
    toEnum n = toEnum (n `mod` 4)
    fromEnum (Left Positive) = 0
    fromEnum (Right Zero) = 1
    fromEnum (Left Negative) = 3
    fromEnum (Right One) = 4

instance Enum YZ where
    toEnum 0 = Left Up
    toEnum 1 = Right Zero
    toEnum 2 = Left Down
    toEnum 3 = Right One
    toEnum n = toEnum (n `mod` 4)
    fromEnum (Left Up) = 0
    fromEnum (Right Zero) = 1
    fromEnum (Left Down) = 3
    fromEnum (Right One) = 4

instance Enum YX where
    toEnum 0 = Left Up
    toEnum 1 = Right Positive
    toEnum 2 = Left Down
    toEnum 3 = Right Negative
    toEnum n = toEnum (n `mod` 4)
    fromEnum (Left Up) = 0
    fromEnum (Right Positive) = 1
    fromEnum (Left Down) = 3
    fromEnum (Right Negative) = 4


-- explicit ignorance : "I don't know"
-- implicit ignorance : "I don't know [ I don't know ]"

-- sqrtNot is a form of explicit ignorance
-- thus possible to exchange ignorance for knowledge

instance Basis ZX where
    falseB   = Left Zero
    trueB    = Left One
    notB     = succ . succ
    sqrtNotB = succ -- ignore X (+ and -)

instance Basis ZY where
    falseB   = Left Zero
    trueB    = Left One
    notB     = succ . succ
    sqrtNotB = succ -- ignore Y (↑ and ↓)

instance Basis XY where
    falseB   = Left Positive
    trueB    = Left Negative
    notB     = succ . succ
    sqrtNotB = succ -- ignore Y (↑ and ↓)

instance Basis XZ where
    falseB   = Left Positive
    trueB    = Left Negative
    notB     = succ . succ
    sqrtNotB = succ -- ignore Z (0 and 1)

instance Basis YZ where
    falseB   = Left Up
    trueB    = Left Down
    notB     = succ . succ
    sqrtNotB = succ -- ignore Z (0 and 1)

instance Basis YX where
    falseB   = Left Up
    trueB    = Left Down
    notB     = succ . succ
    sqrtNotB = succ -- ignore X (+ and -)


z2zx :: Z -> ZX
z2zx = Left

x2xy :: X -> XY
x2xy = Left

y2yz :: Y -> YZ
y2yz = Left


explicateX :: ZX -> XY
explicateX b = case b of
    Right Positive -> Left Positive
    Right Negative -> Left Negative
    _ -> error "cannot explicate."


explicateY :: XY -> YZ
explicateY b = case b of
    Right Up   -> Left Up
    Right Down -> Left Down
    _ -> error "cannot explicate."

explicateZ :: YZ -> ZX
explicateZ b = case b of
    Right Zero -> Left Zero
    Right One  -> Left One
    _ -> error "cannot explicate."


main :: IO ()
main = do
    return ()