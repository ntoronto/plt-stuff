{-# LANGUAGE
    TypeFamilies,
    MultiParamTypeClasses #-}

module Set where

class Eq s => Set s where
  type MemberType x :: *
  empty :: s
  universe :: s
  meet :: s -> s -> s
  join :: s -> s -> s
  contains :: s -> MemberType s -> Bool
  singleton :: MemberType s -> s

--data CompleteSet s = EmptySet | UnivSet | Nonextremal s

