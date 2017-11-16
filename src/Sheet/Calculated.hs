{-# LANGUAGE GADTs, DataKinds #-}

module Sheet.Calculated where

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
  AbilityScores { str :: AbScore
                , dex :: AbScore
                , con :: AbScore
                , int :: AbScore
                , wis :: AbScore
                , cha :: AbScore
                } deriving (Eq, Show)

type AbScore = Int

abilityMod :: AbScore -> Int
abilityMod n = (n`div`2) - 5

getScore :: AbilityScores -> AScore -> AbScore
getScore x k =
  (!!fromEnum k) $ map ($x) [str, con, dex, int, wis, cha]

-- Skills

data SkillBlock =
  SkillBlock { ranksPerLevel :: Int
             , values :: [SkillStat]
             } deriving (Eq, Show, Read)

type SkillName = String

data SkillStat =
  Skill { skillname :: SkillName
        , classSkill :: Bool
        , acpen :: Bool
        , trained :: Bool
        , ability :: AScore
        , ranks :: Int
        } deriving (Eq, Show, Read)

-- Statistical information

data Stats =
  Stats { initiative :: Initiative
        , vitals :: Vitals
        , ac :: ArmorClass
        , saves :: SavingThrows
        , bonuses :: AttackBonuses
        } deriving (Eq, Show, Read)

--- Initiative

type Initiative = (Int, Int)

genInitiative :: Int -> AbilityScores -> Initiative
genInitiative n x = (dex x, n)

initTotal :: Initiative -> Int
initTotal = uncurry (+)

--- Vital statistics

type MaxCurrent = (Int, Int)

data Vitals =
  Vitals { stamina :: MaxCurrent
         , hp :: MaxCurrent
         , resolve :: MaxCurrent
         } deriving (Eq, Show, Read)

--- ArmorClass

-- | ArmorClass
--   Header record type storing the miscellaneous modifiers
--   for each type of armor class, as well as DR and resistances
data ArmorClass =
  ArmorClass { eacMisc :: Int
             , kacMisc :: Int
             , dr :: Int
             , resist :: [Resistance]
             } deriving (Eq, Show, Read)

type Resistance = (DamageType, Int)

acDexMod :: Armor -> AbilityScores -> Int
acDexMod arm ab = min (maxdex arm) (dex ab)

baseAC :: Int
baseAC = 10

cmdBonus :: Int
cmdBonus = 8

eac, kac :: Armor -> AbilityScores -> ArmorClass -> Int
eac arm ab ac = baseAC + armorEAC arm + acDexMod arm ab + eacMisc ac
kac arm ab ac = baseAC + armorKAC arm + acDexMod arm ab + kacMisc ac

--- Saving Throws

data SavingThrows =
  Saves { fort :: Save
        , ref  :: Save
        , will :: Save
        } deriving (Eq, Show, Read)

data Save where
  Fortitude, Reflex, Will :: Int -> Int -> Save
  deriving (Show, Read, Eq)

calculateSave :: Save -> AbilityScores -> Int
calculateSave (Fortitude x y) ab = x + y + (con ab)
calculateSave (Reflex x y) ab = x + y + (dex ab)
calculateSave (Will x y) ab = x + y + (wis ab)

--

data AttackBonuses =
  AttackBonuses { bab :: Int
                , meleeMisc :: Int
                , rangedMisc :: Int
                , thrownMisc :: Int
                } deriving (Read, Show, Eq)


getAttackBonus :: AttackType -> AttackBonuses -> AbilityScores -> Int
getAttackBonus Melee = meleeBonus
getAttackBonus Ranged = rangedBonus
getAttackBonus Thrown = thrownBonus

meleeBonus, rangedBonus, thrownBonus :: AttackBonuses -> AbilityScores -> Int
meleeBonus bon ab = ((+) <$> bab <*> meleeMisc $ bon) + str ab
rangedBonus bon ab = ((+) <$> bab <*> rangedMisc $ bon) + dex ab
thrownBonus bon ab = ((+) <$> bab <*> thrownMisc $ bon) + str ab

type Experience = Int
