{-# LANGUAGE TypeFamilies #-}

module App.Unified where

import Graphics.UI.Threepenny.Core

data Threepenny

data ReactiveBanana

class Runtime a where
  type App a :: *
  run :: App a -> IO ()

instance Runtime Threepenny where
  type App Threepenny = (Window -> UI ())
  run = startGUI defaultConfig { jsPort = Just 10000 }

instance Runtime ReactiveBanana where
  type App ReactiveBanana = IO ()
  run = start
