{-# LANGUAGE TemplateHaskell #-}
module Sheet.Meta where

import Control.Lens

type Classes = String
type Race = String
type Theme = String
type Size = String
type Speed = String
type Gender = String
type Homeworld = String
type Alignment = String
type Deity = String

data CharMeta =
  CharMeta { _name :: String
           , _classes :: Classes
           , _race :: Race
           , _theme :: Theme
           , _size :: Size
           , _speed :: Speed
           , _gender :: Gender
           , _homeworld :: Homeworld
           , _alignment :: Alignment
           , _deity :: Deity
           , _player :: String
           , _description :: [String]
           } deriving (Eq, Show, Read)

makeLenses ''CharMeta
