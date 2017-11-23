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
import Widgets.Recorder


app :: IO ()
app = do
  w <- frame [ text := "Character Meta" ]
  nav <- table w []
  content <- table w []
  let c = _tab content

  let networkDescription :: MomentIO ()
      networkDescription = mdo
           field <- field' c "Foo" bVal
           lrecord <- recorder c bVal (pure "Save") "Load" (pure "dump.dump")
           bVal <- stepper "" $ priorityUnion [ portents field, portents lrecord ]

           liftIO $ set c [ layout := margin 10 $ column 5 [widget field, widget lrecord ] ]

  network <- compile networkDescription
  actuate network
