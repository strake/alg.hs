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
