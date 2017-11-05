module Sheet where

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

type Classes = String
type Race = String
type Theme = String
type Size = String
type Speed = String
type Gender = String
type Homeworld = String
type Alignment = String
type Deity = String

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

data SkillBlock =
  SkillBlock { ranks :: Int
             , values :: [SkillStat]
             } deriving (Eq, Show)

type SkillName = String

data AScore = Strength
            | Dexterity
            | Constitution
            | Intelligence
            | Wisdom
            | Charisma
            deriving (Eq, Ord, Read, Show, Enum)

getScore :: AbilityScores -> AScore -> AbScore
getScore x k =
  (!!fromEnum k) $ map ($x) [str, con, dex, int, wis, cha]


data SkillStat =
  Skill { skillname :: SkillName
        , classSkill :: Bool
        , acpen :: Bool
        , trained :: Bool
        , ability :: AScore
        , ranks :: Int
        }

data Stats
data Weapons
data Armor
data Abilities
data Feats
type Languages = [String]
data Equipment
data Experience
