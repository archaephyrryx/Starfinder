{-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving, FlexibleInstances #-}

module Sheet.Itemized where

import Sheet.Common
import Data.List
import Data.Char

type Energy = Bool

data WeaponType = Natural
                | Basic
                | Advanced
                | SmallArms
                | Longarms
                | HeavyWep
                | Grenade
                deriving (Eq, Show, Ord, Enum, Read)

weaponAttack :: WeaponType -> AttackType
weaponAttack x = case x of
                   Natural -> Melee
                   Basic -> Melee
                   Advanced -> Melee
                   SmallArms -> Ranged
                   Longarms -> Ranged
                   HeavyWep -> Ranged
                   Grenade -> Thrown


type Weapons = [Weapon]

data Die = D4
         | D6
         | D8
         | D10
         | D12
         | D20
         deriving (Eq, Ord, Enum, Show, Read)

type Count = Int

data Dice = Dice Count Die
  deriving (Eq, Ord)

instance Show Dice where
  show (Dice i x) = (show i)++(map toLower . show $ x)

instance Read Dice where
  readsPrec _ str =
    let (count,face) = span isDigit str
     in [(Dice (read count) (read . map toUpper $ str), "")]

data Damage = Dmg { dice :: Dice, dmgBonus :: Int }
  deriving (Eq, Read)

instance Show Damage where
  show x = show (dice x) ++ '+':(show (dmgBonus x))

type CritEffects = [String]

data DamageType = Bludgeoning
                | Piercing
                | Slashing
                | Acid
                | Cold
                | Electricity
                | Fire
                | Sonic
                deriving (Eq, Ord, Read, Show, Enum)

type SpecialProperties = [SpecialProperty]

type SpecialProperty = String

data Weapon =
  MeleeWeapon { weptype :: WeaponType
              , powered :: Energy
              , level :: Int
              , abMisc :: Int
              , damage :: Damage
              , crit :: CritEffects
              , dmgType :: DamageType
              , special :: SpecialProperties
              }
              deriving (Eq, Read, Show)

data Armor = Armor { armorKAC :: Int
                   , armorEAC :: Int
                   , maxdex :: Int
                   , penalty :: Int
                   } deriving (Eq, Read, Show)

type Abilities = [Ability]

data AbilityType = Extraordinary
                 | Supernatural
                 | SpellLike
                 deriving (Ord, Eq, Enum, Read, Show)

data Ability = Ability { abilityName :: String
                       , abilityType :: AbilityType
                       , description :: String
                       } deriving (Eq, Read, Show)


type Feats = [Feat]

data Feat = Feat { featName :: String
                 , isCombat :: Bool
                 , featDescription :: String
                 } deriving (Eq, Show, Read)

type Languages = [String]
type Equipment = [GearItem]

data GearCategory = GearWeapon
                  | GearArmor
                  | GearAugmentation
                  | GearComputer
                  | GearTechnological
                  | GearMagic
                  | GearHybrid
                  | GearVehicle
                  | GearPersonal
                  | GearMedicinal
                  | GearDrug
                  | GearPoison
                  | GearTradeGood
                  | GearConsumable
                  deriving (Eq, Enum, Ord, Show, Read)

data Bulk = Negligible
          | Light
          | Mass Int
          deriving (Ord, Eq, Show, Read)

data GearItem = GearItem { category :: GearCategory
                         , gearlevel :: Int
                         , gearBulk :: Bulk
                         , gearQuantity :: Int
                         } deriving (Eq, Show, Read)
