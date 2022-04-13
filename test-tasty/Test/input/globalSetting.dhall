let MeshPath_ =
      < VTUPath : { vtuPath : Text }
      | CSVDataPath : { pointData : Text, connectivity : Text }
      >

in  { MeshPath = MeshPath_ }
