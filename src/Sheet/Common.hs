{-# LANGUAGE TemplateHaskell #-}
module Sheet.Common where

import Data.Default
import Control.Lens

data AttackType = Melee
                | Ranged
                | Thrown
                deriving (Read, Show, Ord, Eq, Enum)


-- Ability Scores

data AScore = Strength
            | Dexterity
            | Constitution
            | Intelligence
            | Wisdom
            | Charisma
            deriving (Eq, Ord, Read, Show, Enum)

data AbilityScores =
  AbilityScores { _str :: AbScore
                , _dex :: AbScore
                , _con :: AbScore
                , _int :: AbScore
                , _wis :: AbScore
                , _cha :: AbScore
                } deriving (Eq, Show, Read)

type AbScore = Int

instance Default AbilityScores where
  def = AbilityScores 10 10 10 10 10 10


abilityMod :: AbScore -> Int
abilityMod n = (n`div`2) - 5

getScore :: AbilityScores -> AScore -> AbScore
getScore x k =
  (!!fromEnum k) $ map ($ x) [_str, _con, _dex, _int, _wis, _cha]

makeLenses ''AbilityScores
