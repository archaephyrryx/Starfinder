{-# LANGUAGE TemplateHaskell #-}
module Sheet where

import Control.Lens
import Sheet.Common
import Sheet.Itemized
import Sheet.Calculated
import Sheet.Meta
import Sheet.Skills

import Data.Default

data Sheet =
  Sheet { _meta :: CharMeta
        , _scores :: AbilityScores
        , _skills :: SkillBlock
        , _stats :: Stats
        , _attacks :: Weapons
        , _armor :: Armor
        , _abilities :: Abilities
        , _feats :: Feats
        , _langs :: Languages
        , _equip :: Equipment
        , _exp :: Experience
        } deriving (Eq, Show, Read)

makeLenses ''Sheet
