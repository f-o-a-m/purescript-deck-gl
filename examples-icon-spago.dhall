{ name = "examples-icon-app"
, dependencies = (./spago.dhall).dependencies
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # ["examples/icon/src/**/*.purs"]
}
