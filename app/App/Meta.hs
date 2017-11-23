{-# LANGUAGE RecursiveDo, RecordWildCards #-}
module App.Meta where

import Sheet
import Sheet.Meta
import Widgets.Core
import Widgets.Table
import Widgets.Input
import Widgets.Text
import Widgets.Fields
import Util (titleCase)
import Widgets.Cast (lbTranspose)

createField :: Window w -> String -> MomentIO Field
createField w str = mdo
  field <- field' w str bVal
  bVal <- stepper "" $ portents field
  return field


createFieldML :: Window w -> String -> MomentIO Field
createFieldML w str = mdo
  field <- fieldML w str bVal
  bVal <- stepper "" $ portents field
  return field



mapLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapLast _ _ [] = []
mapLast _ g [x] = [g x]
mapLast f g (x:xs) = f x : mapLast f g xs

fields :: [String]
fields = map titleCase ["name", "classes", "race", "theme", "size", "speed", "gender", "homeworld", "alignment", "deity", "player", "description"]

app :: IO ()
app = do
  w <- frame [ text := "Character Meta" ]
  nav <- table w []
  content <- table w []
  let c = _tab content

  debug <- staticText c []

  let networkDescription :: MomentIO ()
      networkDescription = mdo
           fMetaFields <- sequence $ mapLast (createField c) (createFieldML c) fields
           let bFieldValues = map omens fMetaFields
               bMeta = metaBehavior bFieldValues


           sink debug [ text :== show <$> bMeta ]

           liftIO $ set c [ layout := margin 10 $ column 5 [row 5 $ map widget fMetaFields, widget debug] ]

  network <- compile networkDescription
  actuate network

metaBehavior :: [Behavior String] -> Behavior CharMeta
metaBehavior bstrs = metaList <$> lbTranspose bstrs

metaList :: [String] -> CharMeta
metaList xs =
  let [_name,_classes,_race,_theme,_size,_speed,_gender,_homeworld,_alignment,_deity,_player,desc] = xs
      _description = lines desc
   in CharMeta{..}

{-
data CharMeta =
  CharMeta { name :: String
           , classes :: Classes
           , race :: Race
           , theme :: Theme
           , size :: Size
           , speed :: Speed
           , gender :: Gender
           , homeworld :: Homeworld
           , alignment :: Alignment
           , deity :: Deity
           , player :: String
           , description :: [String]
           } deriving (Eq, Show)
           -}
