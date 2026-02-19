module Main (main) where

import qualified CHSH.PR as CHSH_PR
import qualified CHSH.Quantum as CHSH_Q
import qualified CHSH.LHV as CHSH_LHV
import qualified CHSH.PRState as CHSH_SHV

main :: IO ()
main = do
    CHSH_SHV.testPRHiddenState_uniform_random >>= print
    CHSH_SHV.noSignalingPRHiddenState_print