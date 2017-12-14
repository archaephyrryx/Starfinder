{-# LANGUAGE RecordWildCards #-}

module App.Threepenny.Weapons where

import Widgets.Threepenny.Core
import qualified Widgets.Threepenny.Core as UI

import Data.List (intercalate)

import Control.Lens hiding (set, (#), element)

import Sheet
import Sheet.Itemized

import Data.Wrapped

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Weapon Stat"
    UI.addStyleSheet window "weapon.css"

    wep <- display sampleWep

    getBody window #+ [ element wep ]

listShow :: [String] -> String
listShow [] = "-"
listShow x = intercalate ", " x

dashShow :: (Show a) => Maybe a -> String
dashShow = maybe "-" show

display :: Weapon -> UI Element
display wep@Weapon{..} = UI.table #+ map (UI.tr #+)
  [ map (\(n,el) -> UI.td # set UI.colspan n #+ [UI.string el])
    [ (3, _wepname)
    , (1, show _level)
    , (1, show _abMisc)
    , (1, show _damage)
    ]
  , map (\(n,el) -> UI.td # set UI.colspan n #+ [UI.string el])
    [ (1, listShow _crit)
    , (1, brief _dmgType)
    , (1, dashShow _range)
    , (2, dashShow _maxusage)
    , (1, listShow _special)
    ]
  ]

sampleWep :: Weapon
sampleWep =
  let _weptype = Longarms
      _powered = True
      _wepname = "Combat Rifle"
      _level = 10
      _abMisc = 9
      _damage = Dmg (Dice 3 D8) 7
      _crit = []
      _dmgType = Piercing
      _range = Just 90
      _maxusage = Just (12,1)
      _special = ["Analog"]
   in Weapon{..}


