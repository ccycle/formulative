{ equation =
  { m = 1.0
  , g = 1.0
  , a = 1.0
  , b = 1.0
  , xinit = 1.0
  , yinit = 0.5
  , pxInit = -0.5
  , pyInit = 0.0
  }
, constrainedSystem.augmentedLagrangianMethodParameters
  =
  { penaltyCoefficient = 1e-12
  , growthRate = 2.0
  , torelance = 1e-8
  , maximumIterationNumber = 10000
  }
, dynamics =
  { label = "time"
  , initialValue = 0.0
  , finalValue = 1.0
  , stepSize = 1e-8
  , interval = 1
  , maximumIterationNumber = 100
  }
}
