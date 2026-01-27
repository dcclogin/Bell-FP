module Config where

--- | synonym for True and False
tt, ff :: Bool
tt = True
ff = False

class Measurement i where
    allMeasurements :: [i]
class Outcome o where
    allOutcomes :: [o]

instance Outcome Bool where
    allOutcomes = [tt, ff]
instance (Outcome o1, Outcome o2) => Outcome (o1, o2) where
    allOutcomes = [ (o1, o2) | o1 <- allOutcomes, o2 <- allOutcomes ]


eval 
    :: (Measurement i1, Measurement i2) 
    => (Outcome o1, Outcome o2) 
    => (Monad m)
    => (i1 -> m o1, i2 -> m o2) 
    -> (i1, i2) 
    -> m (o1, o2)
eval (f1, f2) (i1, i2) = (,) <$> (f1 i1) <*> (f2 i2)