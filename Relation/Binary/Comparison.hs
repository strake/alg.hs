{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Relation.Binary.Comparison where

import Prelude (Integer)
import qualified Prelude

import Algebra
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Either
import Data.Function (flip, on)
import Data.Maybe
import Data.Ord (Ordering (..))
import Numeric.Natural

infix 4 ≤, ≥, <, >, ≡, ≢

class Preord a where
    (≤), (≥), (<), (>) :: a -> a -> Bool
    (≤) = flip (≥)
    (≥) = flip (≤)
    a < b = a ≤ b && not (a ≥ b)
    (>) = flip (<)

class PartialEq a where
    (≡), (≢) :: a -> a -> Bool
    a ≡ b = not (a ≢ b)
    a ≢ b = not (a ≡ b)

class (Preord a, PartialEq a) => Eq a

class (Preord a, PartialEq a) => PartialOrd a where
    tryCompare :: a -> a -> Maybe Ordering
    tryCompare a b = case (a ≤ b, b ≤ a) of
        (False, False) -> Nothing
        (False, True)  -> Just GT
        (True,  False) -> Just LT
        (True,  True)  -> Just EQ

class (PartialOrd a, Eq a) => Ord a where
    compare :: a -> a -> Ordering
    compare a b = fromJust (tryCompare a b)

instance Preord () where () ≤ () = True
instance PartialEq () where () ≡ () = True
instance PartialOrd () where tryCompare () () = Just EQ
instance Eq ()
instance Ord ()

instance Preord Bool where (≤) = (Prelude.<=)
instance PartialEq Bool where (≡) = (Prelude.==)
instance PartialOrd Bool where tryCompare a b = Just (Prelude.compare a b)
instance Eq Bool
instance Ord Bool

instance Preord Ordering where (≤) = (Prelude.<=)
instance PartialEq Ordering where (≡) = (Prelude.==)
instance PartialOrd Ordering where tryCompare a b = Just (Prelude.compare a b)
instance Eq Ordering
instance Ord Ordering

instance Preord Natural where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Natural where (≡) = (Prelude.==)
instance PartialOrd Natural where tryCompare a b = Just (Prelude.compare a b)
instance Eq Natural
instance Ord Natural

instance Preord Integer where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Integer where (≡) = (Prelude.==)
instance PartialOrd Integer where tryCompare a b = Just (Prelude.compare a b)
instance Eq Integer
instance Ord Integer

instance (PartialEq a, PartialEq b) => PartialEq (a, b) where
    (aₗ, bₗ) ≡ (aᵣ, bᵣ) = aₗ ≡ aᵣ && bₗ ≡ bᵣ
instance (Preord a, Preord b) => Preord (a, b) where
    (aₗ, bₗ) ≤ (aᵣ, bᵣ) = aₗ ≤ aᵣ && bₗ ≤ bᵣ
instance (PartialOrd a, PartialOrd b) => PartialOrd (a, b) where
    tryCompare (aₗ, bₗ) (aᵣ, bᵣ) = liftA2 (,) (tryCompare aₗ aᵣ)
                                              (tryCompare bₗ bᵣ) >>= \ case
        (EQ, y)  -> Just y
        (x,  EQ) -> Just x
        (LT, LT) -> Just LT
        (GT, GT) -> Just GT
        _        -> Nothing

instance (PartialOrd a, PartialOrd b) => Preord (Lexical (a, b)) where
    a ≤ b = Just GT ≢ tryCompare a b
    a < b = Just LT ≡ tryCompare a b
instance (PartialOrd a, PartialOrd b) => PartialOrd (Lexical (a, b)) where
    Lexical (aₗ, bₗ) `tryCompare` Lexical (aᵣ, bᵣ) =
        tryCompare aₗ aᵣ <> tryCompare bₗ bᵣ
instance (PartialOrd a, PartialOrd b, Eq a, Eq b) => Eq (Lexical (a, b))
instance (Ord a, Ord b) => Ord (Lexical (a, b))

instance (Preord a, Preord b) => Preord (Either a b) where
    Left  x ≤ Left  y = x ≤ y
    Right x ≤ Right y = x ≤ y
    _       ≤ _       = False
instance (PartialEq a, PartialEq b) => PartialEq (Either a b) where
    Left  x ≡ Left  y = x ≡ y
    Right x ≡ Right y = x ≡ y
    _       ≡ _       = False
instance (PartialOrd a, PartialOrd b) => PartialOrd (Either a b) where
    Left  x `tryCompare` Left  y = x `tryCompare` y
    Right x `tryCompare` Right y = x `tryCompare` y
    _       `tryCompare` _       = Nothing

instance (Preord a, Preord b) => Preord (Lexical (Either a b)) where
    Lexical (Left _) ≤ Lexical (Right _) = True
    Lexical x ≤ Lexical y = x ≤ y
instance (PartialOrd a, PartialOrd b) => PartialOrd (Lexical (Either a b)) where
    Lexical (Left _) `tryCompare` Lexical (Right _) = Just LT
    Lexical (Right _) `tryCompare` Lexical (Left _) = Just GT
    Lexical x `tryCompare` Lexical y = tryCompare x y
instance (Eq a, Eq b) => Eq (Lexical (Either a b))
instance (Ord a, Ord b) => Ord (Lexical (Either a b))

newtype Lexical a = Lexical a deriving (PartialEq, Semigroup, Monoid, Group)

instance PartialEq a => PartialEq (Maybe a) where (≡) = (≡) `on` maybe (Left ()) Right
instance Preord a => Preord (Maybe a) where (≤) = (≤) `on` maybe (Left ()) Right
instance PartialOrd a => PartialOrd (Maybe a) where
    tryCompare = tryCompare `on` maybe (Left ()) Right
instance Eq a => Eq (Maybe a)
