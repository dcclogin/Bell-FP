module Main (main) where

import CHSH.LHV
import CHSH.Superdet
import CHSH.Quantum
import CHSH.PR
import CHSH.PRState

data ModelType
  = LHVM
  | SuperdetM
  | QuantumM
  | PRM
  | PRHiddenM
  deriving (Eq, Show)

data SamplerType
  = Uniform
  | Plus2
  | Minus2
  | BadIllegal
  | BadLegal
  deriving (Eq, Show)

data SchedulerType
  = Fixed
  | Random
  deriving (Eq, Show)


modelName :: ModelType -> String
modelName LHVM       = "LHV"
modelName SuperdetM  = "Superdet"
modelName QuantumM   = "Quantum"
modelName PRM        = "PR box"
modelName PRHiddenM  = "PR hidden state"

samplerName :: SamplerType -> String
samplerName Uniform            = "uniform"
samplerName Plus2              = "plus2"
samplerName Minus2             = "minus2"
samplerName BadIllegal         = "bad illegal"
samplerName BadLegal           = "bad legal"

schedulerName :: SchedulerType -> String
schedulerName Fixed  = "fixed"
schedulerName Random = "random"

-------------------------------------------------------------
-- a record of implemented configurations

type Config = (ModelType, Maybe SamplerType, Maybe SchedulerType)

configs :: [Config]
configs =
  [ (LHVM, Just Uniform, Just Fixed)
  , (LHVM, Just Uniform, Just Random)
  , (LHVM, Just Plus2, Just Fixed)
  , (LHVM, Just Minus2, Just Fixed)

  , (SuperdetM, Just BadIllegal, Just Fixed)
  , (SuperdetM, Just BadIllegal, Just Random)
  , (SuperdetM, Just BadLegal, Just Fixed)

  , (QuantumM, Nothing, Just Fixed)
  , (QuantumM, Nothing, Just Random)

  , (PRM, Nothing, Just Fixed)
  , (PRM, Nothing, Just Random)

  , (PRHiddenM, Just Uniform, Just Fixed)
  , (PRHiddenM, Just Uniform, Just Random)
  ]

models :: [ModelType]
models =
  [ LHVM
  , SuperdetM
  , QuantumM
  , PRM
  , PRHiddenM
  ]


computeCHSH :: Config -> IO Double
computeCHSH (model, sampler, scheduler) =
  case model of

    ------------------------------------------------
    LHVM ->
      case (sampler, scheduler) of
        (Just Uniform, Just Fixed)  -> pure testLHV_uniform_fixed
        (Just Uniform, Just Random) -> testLHV_uniform_random
        (Just Plus2,  Just Fixed)   -> pure testLHV_plus2
        (Just Minus2, Just Fixed)   -> pure testLHV_minus2
        _ -> pure 0

    ------------------------------------------------
    SuperdetM ->
      case (sampler, scheduler) of
        (Just BadIllegal, Just Fixed)  -> pure testSuperdet_fixed
        (Just BadIllegal, Just Random) -> testSuperdet_random
        (Just BadLegal,  Just Fixed)   -> pure testSuperdet_control
        _ -> pure 0

    ------------------------------------------------
    QuantumM ->
      case scheduler of
        Just Fixed  -> testQuantum_fixed
        Just Random -> testQuantum_random
        _ -> pure 0

    ------------------------------------------------
    PRM ->
      case scheduler of
        Just Fixed  -> testPR_fixed
        Just Random -> testPR_random
        _ -> pure 0

    ------------------------------------------------
    PRHiddenM ->
      case (sampler, scheduler) of
        (Just Uniform, Just Fixed)  -> testPRHiddenState_uniform_fixed
        (Just Uniform, Just Random) -> testPRHiddenState_uniform_random
        _ -> pure 0



computeNSModel :: ModelType -> IO Bool
computeNSModel model =
  case model of
    LHVM -> do
      (ok, _) <- noSignalingLHV_uniform
      pure ok
    SuperdetM -> do
      (ok, _) <- noSignalingSuperdet
      pure ok
    QuantumM -> do
      (ok, _) <- noSignalingQuantum
      pure ok
    PRM -> do
      (ok, _) <- noSignalingPR
      pure ok
    PRHiddenM -> do
      (ok, _) <- noSignalingPRHiddenState
      pure ok


printCHSHRow :: Config -> Double -> IO ()
printCHSHRow (m, ms, sc) v =
  putStrLn $
    pad 20 (modelName m) ++
    pad 15 (maybe "-" samplerName ms) ++
    pad 12 (maybe "-"schedulerName sc) ++
    show v
  where
    pad n str = take n (str ++ repeat ' ')

printNSRow :: ModelType -> Bool -> IO ()
printNSRow m ok =
  putStrLn $
    pad 20 (modelName m) ++
    nsMark
  where
    pad n str = take n (str ++ repeat ' ')
    nsMark = if ok then "✓" else "✗"


printCHSHTable :: IO ()
printCHSHTable = do
  putStrLn ""
  putStrLn "CHSH Values"
  putStrLn "--------------------------------------------------------"
  putStrLn "Model               Sampler        Scheduler   CHSH"
  putStrLn "--------------------------------------------------------"

  mapM_
    (\cfg -> do
        v <- computeCHSH cfg
        printCHSHRow cfg v
    )
    configs


printNSTable :: IO ()
printNSTable = do
  putStrLn ""
  putStrLn "No-Signaling Check"
  putStrLn "---------------------------"
  putStrLn "Model               NS"
  putStrLn "---------------------------"

  mapM_
    (\m -> do
        ok <- computeNSModel m
        printNSRow m ok
    )
    models


main :: IO ()
main = do 
  printCHSHTable
  printNSTable