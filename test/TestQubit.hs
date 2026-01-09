module TestQubit where

import Test.HUnit
import Qubit
import Ontic
import EpState
import Utils
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.IO.Class
import Control.Monad.State.Lazy



-- reproducible measurement outcomes
idempotentEp :: M (Bool, Bool)
idempotentEp = do
    put =<< liftIO randomOntic
    b1 <- qubit zT
    b2 <- qubit zT
    return (b1, b2)


testIdempotent :: Test
testIdempotent = TestCase $ do
    o <- randomOntic
    results <- mapM (\_ -> evalStateT idempotentEp o) [1..100]
    let allSame = all (\(b1, b2) -> b1 == b2) results
    assertBool "Idempotent measurements should yield the same result" allSame