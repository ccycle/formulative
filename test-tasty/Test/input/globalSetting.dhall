let MeshPath_ =
      < MkVTUPath : { vtuPath : Text }
      | MkCSVDataPath : { pointData : Text, connectivity : Text }
      >

in  { MeshPath = MeshPath_ }
