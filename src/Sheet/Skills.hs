{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Sheet.Skills where
--
-- Skills

import Control.Lens
import Sheet.Common

data SkillBlock =
  SkillBlock { _rpl :: Int
             , _values :: SkillStats
             } deriving (Eq, Show, Read)


data Skill = Acrobatics
           | Athletics
           | Bluff
           | Computers
           | Culture
           | Diplomacy
           | Disguise
           | Engineering
           | Intimidate
           | LifeScience
           | Medicine
           | Mysticism
           | Perception
           | PhysicalScience
           | Piloting
           | Profession
           | SenseMotive
           | SleightOfHand
           | Stealth
           | Survival
           deriving (Enum, Ord, Eq, Read, Show)

trainedSkills, penaltySkills :: [Skill]
trainedSkills =
    [ Computers
    , Culture
    , Engineering
    , LifeScience
    , Medicine
    , Mysticism
    , PhysicalScience
    , Profession
    , SleightOfHand]
penaltySkills =
    [ Acrobatics
    , Athletics
    , SleightOfHand
    , Stealth
    ]

trained, penalty :: Skill -> Bool
trained = flip elem trainedSkills
penalty = flip elem penaltySkills

skillScore :: Skill -> AScore
skillScore x = case x of
                 Acrobatics -> Dexterity
                 Athletics -> Strength
                 Bluff -> Charisma
                 Computers -> Intelligence
                 Culture -> Intelligence
                 Diplomacy -> Charisma
                 Disguise -> Charisma
                 Engineering -> Intelligence
                 Intimidate -> Charisma
                 LifeScience -> Intelligence
                 Medicine -> Intelligence
                 Mysticism -> Wisdom
                 Perception -> Wisdom
                 PhysicalScience -> Intelligence
                 Piloting -> Dexterity
                 SenseMotive -> Wisdom
                 SleightOfHand -> Dexterity
                 Stealth -> Dexterity
                 Survival -> Wisdom
                 Profession -> error "Profession has no defined ability score"

data SkillStats =
  SkillStats { _acrobatics :: SkillStat
             , _athletics :: SkillStat
             , _bluff :: SkillStat
             , _computers :: SkillStat
             , _culture :: SkillStat
             , _diplomacy :: SkillStat
             , _disguise :: SkillStat
             , _engineering :: SkillStat
             , _intimidate :: SkillStat
             , _lifeScience :: SkillStat
             , _medicine :: SkillStat
             , _mysticism :: SkillStat
             , _perception :: SkillStat
             , _physicalScience :: SkillStat
             , _piloting :: SkillStat
             , _profession :: SkillStat
             , _senseMotive :: SkillStat
             , _sleightOfHand :: SkillStat
             , _stealth :: SkillStat
             , _survival :: SkillStat
             } deriving (Eq, Show, Read, Ord)


type SkillList = [SkillStat]


fromList :: SkillList -> SkillStats
fromList [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t] = SkillStats a b c d e f g h i j k l m n o p q r s t

toList :: SkillStats -> SkillList
toList (SkillStats a b c d e f g h i j k l m n o p q r s t) = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t]

baseStats :: SkillStats
baseStats = fromList $ map baseStat [Acrobatics .. Survival]

baseStat :: Skill -> SkillStat
baseStat x = let y = case x of { Profession -> Umbrella x [] ; _ -> Standard x }
              in unskilled y

unskilled :: SkillBase -> SkillStat
unskilled s = Skill s False 0 0

data SkillStat =
    Skill { _skillname :: SkillBase
          , _classSkill :: Bool
          , _ranks :: Int
          , _miscmod :: Int
          } deriving (Eq, Show, Read, Ord)


data SkillBase = Standard { _skill :: Skill }
               | Umbrella { _skill :: Skill
                          , _instances :: [(String, AScore)]
                          } deriving (Eq, Show, Read, Ord)

makeLenses ''SkillBlock
makeLenses ''SkillStat
makeLenses ''SkillBase
