let globalSettings = ./globalSetting.dhall

in  { parameters = { a = 1.0, b = 2.0 }
    , meshDataPath =
        globalSettings.MeshPath.MkCSVDataPath
          { pointData = "./pointData.csv", connectivity = "./connectivity.csv" }
    }
