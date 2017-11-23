{-# LANGUAGE TemplateHaskell #-}
module Sheet where

import Control.Lens
import Sheet.Itemized
import Sheet.Calculated
import Sheet.Meta

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
        } deriving (Eq, Show)

makeLenses ''Sheet
