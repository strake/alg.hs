module Data.BitSet where

import Control.Applicative
import Control.Monad (guard)
import Data.Bits
import Data.Maybe
import Prelude ((==))
import qualified Prelude as Base
import Util
import Util.Bits

import Algebra
import Relation.Binary.Comparison

newtype BitSet a = BitSet { bits :: a }
  deriving (Base.Eq, Bits, FiniteBits, Base.Read, Base.Show)

instance Bits a => Preord (BitSet a) where
    BitSet x ≤ BitSet y = x .&. y == x

instance Bits a => PartialEq (BitSet a) where (≡) = (==)

instance Bits a => Eq (BitSet a)
instance Bits a => PartialOrd (BitSet a)

instance Bits a => Semigroup (BitSet a) where (<>) = xor
instance Bits a => Abelian (BitSet a)
instance Bits a => Monoid (BitSet a) where
    mappend = (<>)
    mempty = zeroBits
instance Bits a => Group (BitSet a) where invert = complement

rangeInclusive :: (PartialOrd a, Bits a, Alternative f) => a -> a -> f a
rangeInclusive = \ x y -> go y x <* guard (x ≤ y)
  where go y x = x <| altMap (go y ∘ setBit x) (setBits (y .&¬ x) :: [_])

(.&?¬) :: (PartialOrd a, Bits a) => a -> a -> Maybe a
a .&?¬ b = a .&¬ b <$ guard (b ≤ a)

instance Bits a => Semigroup (Min (BitSet a)) where Min a <> Min b = Min (a .&. b)
instance Bits a => Semigroup (Max (BitSet a)) where Max a <> Max b = Max (a .|. b)

instance Bits a => Abelian (Min (BitSet a))
instance Bits a => Abelian (Max (BitSet a))

instance Bits a => Idempotent (Min (BitSet a))
instance Bits a => Idempotent (Max (BitSet a))

instance Bits a => Monoid (Min (BitSet a)) where
    mappend = (<>)
    mempty = Min (complement zeroBits)
instance Bits a => Monoid (Max (BitSet a)) where
    mappend = (<>)
    mempty = Max zeroBits

instance Bits a => Preord (Min (BitSet a)) where
    Min (BitSet a) ≤ Min (BitSet b) = a .&. b == a
instance Bits a => Preord (Max (BitSet a)) where
    Max (BitSet a) ≤ Max (BitSet b) = a .&. b == b

instance Bits a => PartialEq (Min (BitSet a)) where (≡) = (==)
instance Bits a => PartialEq (Max (BitSet a)) where (≡) = (==)
instance Bits a => Eq (Min (BitSet a))
instance Bits a => Eq (Max (BitSet a))
instance Bits a => PartialOrd (Min (BitSet a))
instance Bits a => PartialOrd (Max (BitSet a))

instance Bits a => Monus (Max (BitSet a)) where monus a b = a .&. complement b
instance Bits a => Monus (Min (BitSet a)) where monus a b = a .|. complement b
