let globalSettings = ./globalSetting.dhall

in  { parameters = { a = 1.0, b = 2.0 }
    , meshPath =
        globalSettings.MeshPath.CSVDataPath
          { pointData = "./pointData.csv", connectivity = "./connectivity.csv" }
    }
