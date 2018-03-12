module Main where

import Prelude
import Control.Monad.Eff.Console

main :: Eff (console :: CONSOLE) Unit
main = log "Example!"
