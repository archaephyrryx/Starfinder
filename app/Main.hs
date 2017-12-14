module Main where

import qualified Widgets.Threepenny.Core as UI
import App.Unified
import App.Threepenny.Weapons (setup)

main :: IO ()
main = run (undefined :: Threepenny) setup
