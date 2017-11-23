{-# LANGUAGE RecursiveDo #-}
module App.Record where

import Sheet
import Sheet.Meta
import Widgets.Core
import Widgets.Input
import Widgets.Text
import Widgets.Table
import Widgets.Fields
import Widgets.Links
import Util (titleCase)
import Widgets.Cast (lbTranspose)

app :: IO ()
app = do
  w <- frame [ text := "Character Meta" ]
  nav <- table w []
  content <- table w []
  let c = _tab content

  let networkDescription :: MomentIO ()
      networkDescription = mdo
           field <- field' c "Foo" bVal
           bVal <- stepper "" $ priorityUnion [ portents field, eLoad ]

           lSave <- (liftIO (preLink c) >>= \l -> liquidLink l (pure $ const "Save") (omens field))
           lLoad <- (liftIO (preLink c) >>= \l -> voidLink l "Load")
           eLoad <- mapEventIO (\_ -> readFile "dump.dump") (portents lLoad)

           lSave`sinksTo`writeFile "dump.dump"

           liftIO $ set c [ layout := margin 10 $ column 5 [widget field, row 5 [ widget lSave, widget lLoad ] ] ]

  network <- compile networkDescription
  actuate network
