let Subdir = List Text

let ExtraDeps =
      < Hackage : Text
      | Git : { git : Text, commit : Text, subdirs : Optional Subdir }
      >

let defaultValue =
      { packages = [ "./", "./formulative-algebra", "./formulative-physics" ]
      , system-ghc = True
      , build =
        { haddock = False
        , haddock-arguments.haddock-args = [ "--odir=haddock" ]
        , open-haddocks = False
        , haddock-internal = False
        , haddock-hyperlink-source = True
        }
      , extra-deps =
        [ ExtraDeps.Git
            { git = "https://github.com/ccycle/minimal-default.git"
            , commit = "640063d85b9f3bb8f8fd1c0be2aba3a60678947c"
            , subdirs = None Subdir
            }
        ]
      }

in  { defaultValue, ExtraDeps, Subdir }
