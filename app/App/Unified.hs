{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, ScopedTypeVariables #-}

module App.Unified where

import Widgets.Threepenny.Core hiding (start, App)
import Widgets.Banana.Core hiding (App, Window)

data Threepenny

data ReactiveBanana

class Runtime a where
  type App a :: *
  run :: a -> App a -> IO ()

instance Runtime Threepenny where
  type App Threepenny = (Window -> UI ())
  run = const (startGUI defaultConfig { jsPort = Just 10000, jsStatic = Just "res/" })

instance Runtime ReactiveBanana where
  type App ReactiveBanana = IO ()
  run = const start
