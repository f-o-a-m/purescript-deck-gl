let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210722/packages.dhall sha256:1ceb43aa59436bf5601bac45f6f3781c4e1f0e4c2b8458105b018e5ed8c30f8c

let overrides = {=}

let additions =
      { react-map-gl =
        { dependencies = [ "prelude", "react", "web-mercator", "simple-json" ]
        , repo = "https://github.com/f-o-a-m/purescript-react-map-gl.git"
        , version = "v0.14"
        }
      , web-mercator =
        { dependencies =
          [ "assert"
          , "console"
          , "effect"
          , "prelude"
          , "psci-support"
          , "partial"
          , "functions"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-web-mercator.git"
        , version = "v0.14"
        }
      }

in  upstream ⫽ overrides ⫽ additions
