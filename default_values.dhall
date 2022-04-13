{ optimization =
  { convergenceTest =
    { numericalError = 1.0e-8
    , convergenceTestType = < AbsoluteError | RelativeError >.RelativeError
    , normType = < Lp : Double | LInfinity >.LInfinity
    }
  , lbfgsParameters = { historySize = 10, maximumIterationNumber = 10000 }
  , lineSearchParameters =
      < FixedStepSize : { stepSize : Double }
      | Backtracking :
          { inequality :
              < Armijo : { c1 : Double }
              | Wolfe : { c1 : Double, c2 : Double }
              | StrongWolfe : { c1 : Double, c2 : Double }
              >
          , factor : Double
          , initialStepSize : Double
          , maximumIterationNumber : Natural
          }
      >.FixedStepSize
        { stepSize = 1.0 }
  }
, export =
  { format = { variable = {=}, local = {=}, global = {=} }
  , output = "./output/[[hash]]"
  }
, geometry =
  { meshPath =
      < NoData
      | VTUPath : { vtuPath : Text }
      | GmshPath : { mshPath : Text }
      | CSVDataPath :
          { pointData : Text, connectivity : Text, boundaryNameList : Text }
      >.NoData
  , metricSignature = < Timelike | Spacelike >.Spacelike
  }
, dynamics =
  { label = "time"
  , initialValue = 0.0
  , finalValue = 1.0
  , stepSize = 1.0e-2
  , interval = 10
  , maximumIterationNumber = 1000
  }
, constrainedSystem.augmentedLagrangianMethodParameters
  =
  { penaltyCoefficient = 1.0e-12
  , growthRate = 2.0
  , torelance = 1.0e-8
  , maximumIterationNumber = 1000
  }
}