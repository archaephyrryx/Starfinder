{-# LANGUAGE RecursiveDo #-}
module App.Calculated where

import Sheet
import Sheet.Calculated
import Widgets.Core hiding (set)
import qualified Widgets.Core as Widgets (set)
import Widgets.Table
import Widgets.Fields
import Widgets.Links
import Util (titleCase)
import Widgets.Recorder
import Control.Lens

createField :: Window w -> String -> MomentIO TextField
createField w str = mdo
  field <- field' w str bVal
  bVal <- stepper "" $ portents field
  return field


createFieldML :: Window w -> String -> MomentIO TextField
createFieldML w str = mdo
  field <- fieldML w str bVal
  bVal <- stepper "" $ portents field
  return field



mapLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapLast _ _ [] = []
mapLast _ g [x] = [g x]
mapLast f g (x:xs) = f x : mapLast f g xs

scoreFields :: Window w -> Behavior AbilityScores -> [MomentIO (Field Int)]
scoreFields c bScores =
  let f s g = intField c s (g <$> bScores)
   in [ f "STR" _str
      , f "DEX" _dex
      , f "CON" _con
      , f "INT" _int
      , f "WIS" _wis
      , f "CHA" _cha
      ]

skillFields :: Window w -> Behavior SkillBlock -> [MomentIO TextField]
skillFields c bSkills = []


statFields :: Window w -> Behavior Stats -> [MomentIO TextField]
statFields c bStats = []


{-
data Mode = Edit | View deriving (Eq, Ord, Enum, Read, Show)

modeFlip :: Mode -> Mode
modeFlip Edit = View
modeFlip View = Edit
-}

app :: IO ()
app = do
  w <- frame [ text := "Calculated" ]
  nav <- table w []
  content <- table w []
  let c = _tab content

  debug <- staticText c []

  let networkDescription :: MomentIO ()
      networkDescription = mdo
           fScoreFields <- sequence $ scoreFields c bScores
           let [fStr, fDex, fCon, fInt, fWis, fCha] = fScoreFields

           -- lMode <- liftIO (preLink c) >>= \l -> liquidLink l (pure show) bMode
           --bMode <- accumB Edit $ modeFlip <$ portents lMode

           bScores <- accumB (AbilityScores 10 10 10 10 10 10) $
             priorityUnion [ const . read <$> portents lrecord
                           , set str <$> portents fStr
                           , set dex <$> portents fDex
                           , set con <$> portents fCon
                           , set int <$> portents fInt
                           , set wis <$> portents fWis
                           , set cha <$> portents fCha
                           ]
           let bVal = show <$> bScores
           lrecord <- recorder c bVal (pure "Save") "Load" (pure "dump.dump")

           sink debug [ text :== show <$> bVal ]

           liftIO $ Widgets.set c [ layout := margin 10 $ column 5 [row 5 $ map widget fScoreFields, widget lrecord, widget debug ] ]

  network <- compile networkDescription
  actuate network

{-


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
                } deriving (Eq, Show)

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
-}
