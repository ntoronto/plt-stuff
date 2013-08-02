{-# LANGUAGE
    TypeFamilies #-}

module Set where

-- A type of class `Set s' denotes members of a bounded lattice of subsets.
-- Members of these subsets are of type `MemberType s'.

infixr 3 /\
infixr 2 \/

class Eq s => Set s where
  type MemberType s :: *
  empty :: s     -- lattice bottom
  universe :: s  -- lattice top
  (/\) :: s -> s -> s
  (\/) :: s -> s -> s
  contains :: s -> MemberType s -> Bool
  singleton :: MemberType s -> s

