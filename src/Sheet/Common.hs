module Sheet.Common where

data AttackType = Melee
                | Ranged
                | Thrown
                deriving (Read, Show, Ord, Eq, Enum)
