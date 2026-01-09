module Spekken.Ontic (
    Ontic(..),
    allOntics,
    PermO(..),
    shiftL,
    shiftR,
    xPermO,
    yPermO,
    zPermO,
    swapPermO,
    -- cnotPermO
) where


-- ontic states

-- O1 ~ ■□□□
-- O2 ~ □■□□
-- O3 ~ □□■□
-- O4 ~ □□□■

data Ontic = O1 | O2 | O3 | O4
    deriving (Show, Eq, Ord, Enum, Bounded)

allOntics :: [Ontic]
allOntics  = [minBound .. maxBound]


--- | define permutations of ontic states
newtype PermO = PermO { runPermO :: Ontic -> Ontic }

shiftL :: PermO
shiftL = PermO $ \o -> case o of
    O1 -> O4
    O2 -> O1
    O3 -> O2
    O4 -> O3

shiftR :: PermO
shiftR = PermO $ \o -> case o of
    O1 -> O2
    O2 -> O3
    O3 -> O4
    O4 -> O1

swap12 :: PermO
swap12 = PermO $ \o -> case o of
    O1 -> O2
    O2 -> O1
    _  -> o

swap34 :: PermO
swap34 = PermO $ \o -> case o of
    O3 -> O4
    O4 -> O3
    _  -> o

swap13 :: PermO
swap13 = PermO $ \o -> case o of
    O1 -> O3
    O3 -> O1
    _  -> o

swap24 :: PermO
swap24 = PermO $ \o -> case o of
    O2 -> O4
    O4 -> O2
    _  -> o

swap14 :: PermO
swap14 = PermO $ \o -> case o of
    O1 -> O4
    O4 -> O1
    _  -> o

-- quasi-hadamard swap
swap23 :: PermO
swap23 = PermO $ \o -> case o of
    O2 -> O3
    O3 -> O2
    _  -> o


--- pauli permutations
xPermO, yPermO, zPermO :: PermO
xPermO = PermO $ runPermO swap13 . runPermO swap24
yPermO = PermO $ runPermO swap14 . runPermO swap23
zPermO = PermO $ runPermO swap12 . runPermO swap34


swapPermO :: Ontic -> Ontic -> PermO
swapPermO O1 O2 = swap12
swapPermO O2 O1 = swap12
swapPermO O3 O4 = swap34
swapPermO O4 O3 = swap34
swapPermO O1 O3 = swap13
swapPermO O3 O1 = swap13
swapPermO O2 O4 = swap24
swapPermO O4 O2 = swap24
swapPermO O1 O4 = swap14
swapPermO O4 O1 = swap14
swapPermO O2 O3 = swap23
swapPermO O3 O2 = swap23
swapPermO _  _  = PermO id


-- product of two ontic states for QubitPair


cnotPermO :: (Ontic, Ontic) -> (Ontic, Ontic)
cnotPermO = h . g . f
    where
        f (a , b) = case (a , b) of
            (O2, b) -> (O2, runPermO swap12 b)
            (O4, b) -> (O4, runPermO swap12 b)
            _       -> (a , b)
        g (a , b) = case (a , b) of
            (a, O3) -> (runPermO swap13 a, O3)
            (a, O4) -> (runPermO swap13 a, O4)
            _       -> (a , b)
        h (a , b) = case (a , b) of
            (O2, O3) -> (O4, O4)
            (O2, O4) -> (O4, O3)
            _        -> (a , b)     