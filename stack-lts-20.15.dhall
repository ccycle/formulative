let stack = ./stack.dhall

in      stack.defaultValue
    //  { resolver = "lts-20.15"
        , extra-deps =
              stack.defaultValue.extra-deps
            # [ stack.ExtraDeps.Hackage
                  "ieee-0.7@sha256:4f444419a7861f04fb64502d14688c80e8293a86a608f6d14c3010c9bd96b6d1,1327"
              , stack.ExtraDeps.Hackage
                  "physics-0.1.2.1@sha256:5555fb0a679d00964f1fbd8bf1a9b233c91b363b371f092077183fe51bcffba6,1467"
              ]
        }
