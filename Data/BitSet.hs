module Data.BitSet where

import Algebra
import Data.Bits
import Prelude ((==))
import qualified Prelude as Base
import Relation.Binary.Comparison

newtype BitSet a = BitSet { bits :: a }
  deriving (Base.Eq, Bits, Base.Read, Base.Show)

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
