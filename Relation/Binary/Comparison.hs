module Relation.Binary.Comparison (module Relation.Binary.Comparison, Ordering (..)) where

import Prelude (Char, Integer)
import qualified Prelude

import Algebra
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Bool
import Data.Either
import Data.Function (flip, on)
import Data.Int
import Data.Maybe
import Data.Monoid (Sum (..))
import Data.Ord (Down (..), Ordering (..))
import Data.Ratio
import Data.Word
import Numeric.Natural

infix 4 ≤, ≥, <, >, ≡, ≢

class Preord a where
    {-# MINIMAL (≥) | (≤) #-}
    (≤), (≥), (<), (>) :: a -> a -> Bool
    (≤) = flip (≥)
    (≥) = flip (≤)
    a < b = a ≤ b && not (a ≥ b)
    (>) = flip (<)

class PartialEq a where
    {-# MINIMAL (≡) | (≢) #-}
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

instance Preord a => Preord (Down a) where
    Down a ≤ Down b = a ≥ b
    Down a ≥ Down b = a ≤ b
    Down a < Down b = a > b
    Down a > Down b = a < b
deriving via a instance PartialEq a => PartialEq (Down a)
instance PartialOrd a => PartialOrd (Down a) where Down a `tryCompare` Down b = tryCompare b a
deriving via a instance Eq a => Eq (Down a)
instance Ord a => Ord (Down a)

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

instance Preord Int where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Int where (≡) = (Prelude.==)
instance PartialOrd Int where tryCompare a b = Just (Prelude.compare a b)
instance Eq Int
instance Ord Int

instance Preord Int8 where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Int8 where (≡) = (Prelude.==)
instance PartialOrd Int8 where tryCompare a b = Just (Prelude.compare a b)
instance Eq Int8
instance Ord Int8

instance Preord Int16 where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Int16 where (≡) = (Prelude.==)
instance PartialOrd Int16 where tryCompare a b = Just (Prelude.compare a b)
instance Eq Int16
instance Ord Int16

instance Preord Int32 where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Int32 where (≡) = (Prelude.==)
instance PartialOrd Int32 where tryCompare a b = Just (Prelude.compare a b)
instance Eq Int32
instance Ord Int32

instance Preord Int64 where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Int64 where (≡) = (Prelude.==)
instance PartialOrd Int64 where tryCompare a b = Just (Prelude.compare a b)
instance Eq Int64
instance Ord Int64

instance Preord Word where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Word where (≡) = (Prelude.==)
instance PartialOrd Word where tryCompare a b = Just (Prelude.compare a b)
instance Eq Word
instance Ord Word

instance Preord Word8 where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Word8 where (≡) = (Prelude.==)
instance PartialOrd Word8 where tryCompare a b = Just (Prelude.compare a b)
instance Eq Word8
instance Ord Word8

instance Preord Word16 where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Word16 where (≡) = (Prelude.==)
instance PartialOrd Word16 where tryCompare a b = Just (Prelude.compare a b)
instance Eq Word16
instance Ord Word16

instance Preord Word32 where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Word32 where (≡) = (Prelude.==)
instance PartialOrd Word32 where tryCompare a b = Just (Prelude.compare a b)
instance Eq Word32
instance Ord Word32

instance Preord Word64 where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Word64 where (≡) = (Prelude.==)
instance PartialOrd Word64 where tryCompare a b = Just (Prelude.compare a b)
instance Eq Word64
instance Ord Word64

instance Preord Char where
    (≤) = (Prelude.<=)
    (<) = (Prelude.<)
instance PartialEq Char where (≡) = (Prelude.==)
instance PartialOrd Char where tryCompare a b = Just (Prelude.compare a b)
instance Eq Char
instance Ord Char

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

instance (PartialEq a, PartialEq b, PartialEq c) => PartialEq (a, b, c) where
    (aₗ, bₗ, cₗ) ≡ (aᵣ, bᵣ, cᵣ) = aₗ ≡ aᵣ && bₗ ≡ bᵣ && cₗ ≡ cᵣ
instance (Preord a, Preord b, Preord c) => Preord (a, b, c) where
    (aₗ, bₗ, cₗ) ≤ (aᵣ, bᵣ, cᵣ) = aₗ ≤ aᵣ && bₗ ≤ bᵣ && cₗ ≤ cᵣ
instance (PartialOrd a, PartialOrd b, PartialOrd c) => PartialOrd (a, b, c) where
    tryCompare (aₗ, bₗ, cₗ) (aᵣ, bᵣ, cᵣ) = tryCompare (aₗ, (bₗ, cₗ)) (aᵣ, (bᵣ, cᵣ))

instance (PartialOrd a, PartialOrd b, PartialOrd c) => Preord (Lexical (a, b, c)) where
    a ≤ b = Just GT ≢ tryCompare a b
    a < b = Just LT ≡ tryCompare a b
instance (PartialOrd a, PartialOrd b, PartialOrd c) => PartialOrd (Lexical (a, b, c)) where
    Lexical (aₗ, bₗ, cₗ) `tryCompare` Lexical (aᵣ, bᵣ, cᵣ) =
        tryCompare aₗ aᵣ <> tryCompare bₗ bᵣ <> tryCompare cₗ cᵣ
instance (PartialOrd a, PartialOrd b, PartialOrd c, Eq a, Eq b, Eq c) => Eq (Lexical (a, b, c))
instance (Ord a, Ord b, Ord c) => Ord (Lexical (a, b, c))

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

instance (Preord a) => Preord (Lexical (Maybe a)) where
    Lexical Nothing ≤ Lexical (Just _) = True
    Lexical x ≤ Lexical y = x ≤ y
instance (PartialOrd a) => PartialOrd (Lexical (Maybe a)) where
    Lexical Nothing `tryCompare` Lexical (Just _) = Just LT
    Lexical (Just _) `tryCompare` Lexical Nothing = Just GT
    Lexical x `tryCompare` Lexical y = tryCompare x y
instance (Eq a) => Eq (Lexical (Maybe a))
instance (Ord a) => Ord (Lexical (Maybe a))

newtype Lexical a = Lexical { unLexical :: a }
  deriving (PartialEq, Semigroup, Monoid, Group) via a

instance PartialEq a => PartialEq (Maybe a) where (≡) = (≡) `on` maybe (Left ()) Right
instance Preord a => Preord (Maybe a) where (≤) = (≤) `on` maybe (Left ()) Right
instance PartialOrd a => PartialOrd (Maybe a) where
    tryCompare = tryCompare `on` maybe (Left ()) Right
instance Eq a => Eq (Maybe a)

class (Monoid a, Abelian a, PartialOrd a) => Monus a where
    monus :: a -> a -> a

deriving via Natural instance Preord (Sum Natural)
deriving via Natural instance PartialEq (Sum Natural)
deriving via Natural instance PartialOrd (Sum Natural)
deriving via Natural instance Eq (Sum Natural)
deriving via Natural instance Ord (Sum Natural)

instance Monus (Sum Natural) where
    0 `monus` _ = 0
    a `monus` 0 = a
    a `monus` b = (a Prelude.- 1) `monus` (b Prelude.- 1)

(∸) :: Monus (Sum a) => a -> a -> a
a ∸ b = getSum (Sum a `monus` Sum b)

max, min :: Ord a => a -> a -> a
max a b | a > b = a | otherwise = b
min a b | a < b = a | otherwise = b

newtype Max a = Max { unMax :: a } deriving (Prelude.Eq, Bits, FiniteBits, Prelude.Read, Prelude.Show) via a
newtype Min a = Min { unMin :: a } deriving (Prelude.Eq, Bits, FiniteBits, Prelude.Read, Prelude.Show) via a

instance {-# OVERLAPPABLE #-} Ord a => Semigroup (Max a) where Max a <> Max b = Max (max a b)
instance {-# OVERLAPPABLE #-} Ord a => Semigroup (Min a) where Min a <> Min b = Min (min a b)

instance PartialEq a => PartialEq (Ratio a) where
    (≡) = (≡) `on` liftA2 (,) numerator denominator

instance PartialEq a => PartialEq [a] where
    [] ≡ [] = True
    x:xs ≡ y:ys = (x, xs) ≡ (y, ys)
    _ ≡ _ = False
