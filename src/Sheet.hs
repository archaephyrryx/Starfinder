module Sheet where

import Sheet.Itemized
import Sheet.Calculated
import Sheet.Meta

data Sheet =
  Sheet { meta :: CharMeta
        , scores :: AbilityScores
        , skills :: SkillBlock
        , stats :: Stats
        , attacks :: Weapons
        , armor :: Armor
        , abilities :: Abilities
        , feats :: Feats
        , langs :: Languages
        , equip :: Equipment
        , exp :: Experience
        } deriving (Eq, Show)

defaultAbilityScores = AbilityScores 10 10 10 10 10 10

blankSheet :: Sheet
blankSheet = Sheet { meta = blankMeta
                   , abilityScores = defaultAbilityScores
                   ,

