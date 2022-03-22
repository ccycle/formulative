{ optimization =
  { convergenceTestParameters =
    { numericalError = 1.0e-8
    , convergenceTestType = < AbsoluteError | RelativeError >.RelativeError
    , normType = < Lp : Double | LInfinity >.LInfinity
    }
  , lbfgsParameters = { historySize = 10, maximumIterationNumber = 100 }
  , lineSearchParameters =
    { inequality =
      { conditionType = < Armijo | Wolfe | StrongWolfe >.Armijo
      , armijoConditionParameter = 1.0e-4
      , wolfeConditionParameter = 0.9
      }
    , maximumStepSize = 1.0
    , backtracking = { factor = 1.0, maximumIterationNumber = 100 }
    }
  }
, export =
  { quantity =
    { local = { script = "", format = < CSV | VTU >.CSV }
    , global = { script = "", format = < CSV | VTU >.CSV }
    }
  , output = "./output"
  }
, geometry =
  { meshPath =
      < NoData
      | MkVTUPath : { vtuPath : Text }
      | MkGmshPath : { mshPath : Text }
      | MkCSVDataPath :
          { pointData : Text, connectivity : Text, boundaryNameList : Text }
      >.NoData
  , dimensionOfManifold = 0
  , dimensionOfEuclideanSpace = 0
  , metricSignature = < Timelike | Spacelike >.Spacelike
  }
, dynamics =
  { label = "time"
  , initialValue = 0.0
  , finalValue = 1.0
  , stepSize = 1.0e-2
  , interval = 10
  , maximumIterationNumber = 100
  }
, constrainedSystem.augmentedLagrangianMethodParameters
  =
  { penaltyCoefficient = 1.0, growthRate = 1.5, maximumIterationNumber = 100 }
}