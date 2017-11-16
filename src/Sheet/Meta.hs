module Sheet.Meta where

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
