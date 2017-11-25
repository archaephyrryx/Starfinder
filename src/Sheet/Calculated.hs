{-# LANGUAGE GADTs, DataKinds, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Sheet.Calculated where

import Control.Lens
import Sheet.Itemized
import Sheet.Common

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


abilityMod :: AbScore -> Int
abilityMod n = (n`div`2) - 5

getScore :: AbilityScores -> AScore -> AbScore
getScore x k =
  (!!fromEnum k) $ map ($ x) [_str, _con, _dex, _int, _wis, _cha]

-- Skills

data SkillBlock =
  SkillBlock { _rpl :: Int
             , _values :: SkillStats
             } deriving (Eq, Show, Read)



type SkillStats = [SkillStat]

type SkillName = String

data SkillStat =
  Skill { _skillname :: SkillName
        , _classSkill :: Bool
        , _acpen :: Bool
        , _trained :: Bool
        , _ability :: AScore
        , _ranks :: Int
        } deriving (Eq, Show, Read)



-- Statistical information

data Stats =
  Stats { _initiative :: Initiative
        , _vitals :: Vitals
        , _ac :: ArmorClass
        , _saves :: SavingThrows
        , _bonuses :: AttackBonuses
        } deriving (Eq, Show, Read)


--- Initiative

type Initiative = (Int, Int)

genInitiative :: Int -> AbilityScores -> Initiative
genInitiative n x = (_dex x, n)

initTotal :: Initiative -> Int
initTotal = uncurry (+)

--- Vital statistics

type MaxCurrent = (Int, Int)

data Vitals =
  Vitals { _stamina :: MaxCurrent
         , _hp :: MaxCurrent
         , _resolve :: MaxCurrent
         } deriving (Eq, Show, Read)


--- ArmorClass

-- | ArmorClass
--   Header record type storing the miscellaneous modifiers
--   for each type of armor class, as well as DR and resistances
data ArmorClass =
  ArmorClass { _eacMisc :: Int
             , _kacMisc :: Int
             , _dr :: Int
             , _resist :: [Resistance]
             } deriving (Eq, Show, Read)


type Resistance = (DamageType, Int)

acDexMod :: Armor -> AbilityScores -> Int
acDexMod arm ab = min (_maxdex arm) (_dex ab)

baseAC :: Int
baseAC = 10

cmdBonus :: Int
cmdBonus = 8

eac, kac :: Armor -> AbilityScores -> ArmorClass -> Int
eac arm ab ac = baseAC + _armorEAC arm + acDexMod arm ab + _eacMisc ac
kac arm ab ac = baseAC + _armorKAC arm + acDexMod arm ab + _kacMisc ac

--- Saving Throws

data SavingThrows =
  Saves { _fort :: Save
        , _ref  :: Save
        , _will :: Save
        } deriving (Eq, Show, Read)


data Save = Fortitude { _base :: Int, _misc :: Int }
          | Reflex { _base :: Int, _misc :: Int }
          | Will { _base :: Int, _misc :: Int }
          deriving (Show, Read, Eq)


calculateSave :: Save -> AbilityScores -> Int
calculateSave Fortitude{..} ab = _base + _misc + _con ab
calculateSave Reflex{..} ab = _base + _misc + _dex ab
calculateSave Will{..} ab = _base + _misc + _wis ab

--

data AttackBonuses =
  AttackBonuses { _bab :: Int
                , _meleeMisc :: Int
                , _rangedMisc :: Int
                , _thrownMisc :: Int
                } deriving (Read, Show, Eq)


getAttackBonus :: AttackType -> AttackBonuses -> AbilityScores -> Int
getAttackBonus Melee = meleeBonus
getAttackBonus Ranged = rangedBonus
getAttackBonus Thrown = thrownBonus

meleeBonus, rangedBonus, thrownBonus :: AttackBonuses -> AbilityScores -> Int
meleeBonus bon ab = ((+) <$> _bab <*> _meleeMisc $ bon) + _str ab
rangedBonus bon ab = ((+) <$> _bab <*> _rangedMisc $ bon) + _dex ab
thrownBonus bon ab = ((+) <$> _bab <*> _thrownMisc $ bon) + _str ab

type Experience = Int

makeLenses ''AbilityScores
makeLenses ''SkillBlock
makeLenses ''SkillStat
makeLenses ''Stats
makeLenses ''Vitals
makeLenses ''ArmorClass
makeLenses ''Save
makeLenses ''SavingThrows
