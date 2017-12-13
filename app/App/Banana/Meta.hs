{-# LANGUAGE RecursiveDo, RecordWildCards #-}
module App.Banana.Meta where

import Sheet
import Sheet.Meta

import Widgets.Banana hiding (set, size, alignment)
import qualified Widgets.Banana as Widgets (set)

import Util (titleCase)
import Control.Lens

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

metaFields :: Window w -> Behavior CharMeta -> [MomentIO Field]
metaFields c bMeta =
  let f s g = field' c s (g <$> bMeta)
   in [ f "Name" _name
      , f "Class" _classes
      , f "Race" _race
      , f "Theme" _theme
      , f "Size" _size
      , f "Speed" _speed
      , f "Gender" _gender
      , f "Homeworld" _homeworld
      , f "Alignment" _alignment
      , f "Deity" _deity
      , f "Player" _player
      , fieldML c "Description" (unlines . _description <$> bMeta)
      ]

app :: IO ()
app = do
  w <- frame [ text := "Character Meta" ]
  nav <- table w []
  content <- table w []
  let c = _tab content

  debug <- staticText c []

  let networkDescription :: MomentIO ()
      networkDescription = mdo
           fMetaFields <- sequence $ metaFields c bMeta
           let [fNam, fCla, fRac, fThe, fSiz, fSpe, fGen, fHom, fAli, fDei, fPla, fDes] = fMetaFields

           bMeta <- accumB (metaList $ replicate 12 "") $
             priorityUnion [ const . read <$> portents lrecord
                           , set name <$> portents fNam
                           , set classes <$> portents fCla
                           , set race <$> portents fRac
                           , set theme <$> portents fThe
                           , set size <$> portents fSiz
                           , set speed <$> portents fSpe
                           , set gender <$> portents fGen
                           , set homeworld <$> portents fHom
                           , set alignment <$> portents fAli
                           , set deity <$> portents fDei
                           , set player <$> portents fPla
                           , set description . lines <$> portents fDes
                           ]
           let bVal = show <$> bMeta
           lrecord <- recorder c bVal (pure "Save") "Load" (pure "dump.dump")

           sink debug [ text :== show <$> bVal ]

           liftIO $ Widgets.set c [ layout := margin 10 $ column 5 [row 5 $ map widget fMetaFields, widget lrecord, widget debug ] ]

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
