{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Sheet.Itemized where

import Control.Lens
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



data Die = D4
         | D6
         | D8
         | D10
         | D12
         | D20
         deriving (Eq, Ord, Enum, Show, Read)

type Count = Int

data Dice = Dice { _count :: Count
                 , _die :: Die
                 } deriving (Eq, Ord)


instance Show Dice where
  show Dice{..} = show _count++(map toLower . show $ _die)

instance Read Dice where
  readsPrec _ str =
    let (count,face) = span isDigit str
     in [(Dice (read count) (read . map toUpper $ str), "")]

makeLenses ''Dice

data Damage = Dmg
  { _dice :: Dice
  , _dmgBonus :: Int
  }
  deriving (Eq, Read)

makeLenses ''Damage

instance Show Damage where
  show Dmg{..} = show _dice ++ '+':show _dmgBonus

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
    Weapon { _weptype :: WeaponType
           , _powered :: Energy
           , _level :: Int
           , _abMisc :: Int
           , _damage :: Damage
           , _crit :: CritEffects
           , _dmgType :: DamageType
           , _range :: Maybe Int
           , _maxusage :: Maybe (Int, Int)
           , _special :: SpecialProperties
           }
           deriving (Eq, Read, Show)

type Weapons = [Weapon]

makeLenses ''Weapon

data Armor = Armor { _armorKAC :: Int
                   , _armorEAC :: Int
                   , _maxdex :: Int
                   , _penalty :: Int
                   } deriving (Eq, Read, Show)

makeLenses ''Armor

type Abilities = [Ability]

data AbilityType = Extraordinary
                 | Supernatural
                 | SpellLike
                 deriving (Ord, Eq, Enum, Read, Show)

data Ability = Ability { _abilityName :: String
                       , _abilityType :: AbilityType
                       , _abilityText :: String
                       } deriving (Eq, Read, Show)

makeLenses ''Ability

type Feats = [Feat]

data Feat = Feat { _featName :: String
                 , _isCombat :: Bool
                 , _featDescription :: String
                 } deriving (Eq, Show, Read)

makeLenses ''Feat

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

data GearItem = GearItem { _category :: GearCategory
                         , _gearlevel :: Int
                         , _gearBulk :: Bulk
                         , _gearQuantity :: Int
                         } deriving (Eq, Show, Read)

makeLenses ''GearItem
