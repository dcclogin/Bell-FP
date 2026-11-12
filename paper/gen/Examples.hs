-- The paper's own examples, compiled against the package. The paper
-- quotes them by line number, and PaperData.hs checks what the paper
-- says about them.
module Examples where

import CHSH.Experiment
import CHSH.LHV (LHV (..), Lambda, Sampler)
import CHSH.Superdet (BadSampler)

import Control.Applicative (liftA2)
import Control.Monad.Reader (runReader)

------------------------------------------------------------
-- The settings and the strategy of one step, drawn together.

-- The sampler cannot see the settings: applicative composition.
independent :: Monad e => Scheduler e -> Sampler e -> Exp e (Context, Lambda)
independent sched samp = liftA2 (,) sched samp

-- The sampler sees them: a bind. This is superdeterminism.
dependent :: Monad e => Scheduler e -> BadSampler e -> Exp e (Context, Lambda)
dependent sched bad = sched >>= \s -> (,) s <$> bad s

------------------------------------------------------------
-- A trial of the local boxes, given the draw: they answer from λ.
-- With 'independent', it is the trial of an LHV model; with
-- 'dependent', that of a superdeterministic one (PaperData.hs checks
-- both against the package's trial).

trialFrom :: Monad e => Exp e (Context, Lambda) -> Exp e Record
trialFrom draw = do
  ((a, b), lam) <- draw
  let (x, y) = runReader (unLHV (jointAB a b)) lam
  pure (Record a b x y)
