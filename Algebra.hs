{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Algebra (Semigroup (..), Monoid (mempty), Group (..), Abelian, Idempotent,
                All, Any, Xor, (+), (-), (*), (/)) where

import Control.Category
import Data.Bits
import Data.Eq
import Data.Functor
import Data.Functor.Const
import Data.Functor.Identity
import Data.Monoid hiding ((<>), All, Any)
import Data.Proxy
import Data.Semigroup hiding (All, Any)
import Data.Word
import Numeric.Natural
import Prelude (Int, Integer)
import qualified Prelude as Base

class Semigroup a => Abelian a

instance Abelian ()
instance (Abelian a, Abelian b) => Abelian (a, b) where
instance (Abelian a, Abelian b, Abelian c) => Abelian (a, b, c) where
instance (Abelian a, Abelian b, Abelian c, Abelian d) => Abelian (a, b, c, d) where
instance (Abelian a, Abelian b, Abelian c, Abelian d, Abelian e) => Abelian (a, b, c, d, e) where

instance Abelian a => Abelian (Identity a)
instance Abelian a => Abelian (Dual a)
instance Abelian (Proxy a)
instance Abelian a => Abelian (Const a b)
instance Abelian b => Abelian (a -> b)

instance Abelian (Sum Natural)
instance Abelian (Sum Integer)
instance Abelian (Sum Word)
instance Abelian (Sum Int)

instance Abelian (Product Natural)
instance Abelian (Product Integer)
instance Abelian (Product Word)
instance Abelian (Product Int)

instance Abelian (Min Natural)
instance Abelian (Min Integer)
instance Abelian (Min Word)
instance Abelian (Min Int)

instance Abelian (Max Natural)
instance Abelian (Max Integer)
instance Abelian (Max Word)
instance Abelian (Max Int)

class Semigroup a => Idempotent a

instance Idempotent ()
instance (Idempotent a, Idempotent b) => Idempotent (a, b) where
instance (Idempotent a, Idempotent b, Idempotent c) => Idempotent (a, b, c) where
instance (Idempotent a, Idempotent b, Idempotent c, Idempotent d) => Idempotent (a, b, c, d) where
instance (Idempotent a, Idempotent b, Idempotent c, Idempotent d, Idempotent e) => Idempotent (a, b, c, d, e) where

instance Idempotent a => Idempotent (Identity a)
instance Idempotent a => Idempotent (Dual a)
instance Idempotent (Proxy a)
instance Idempotent a => Idempotent (Const a b)
instance Idempotent b => Idempotent (a -> b)

instance Base.Ord a => Idempotent (Min a)
instance Base.Ord a => Idempotent (Max a)

class Monoid a => Group a where
    invert :: a -> a

instance Group () where
    invert () = ()

instance (Group a, Group b) => Group (a, b) where
    invert (a, b) = (invert a, invert b)

instance (Group a, Group b, Group c) => Group (a, b, c) where
    invert (a, b, c) = (invert a, invert b, invert c)

instance (Group a, Group b, Group c, Group d) => Group (a, b, c, d) where
    invert (a, b, c, d) = (invert a, invert b, invert c, invert d)

instance (Group a, Group b, Group c, Group d, Group e) => Group (a, b, c, d, e) where
    invert (a, b, c, d, e) = (invert a, invert b, invert c, invert d, invert e)

instance Group a => Group (Identity a) where invert = fmap invert

instance Group a => Group (Dual a) where invert (Dual a) = Dual (invert a)

instance Group (Proxy a) where invert Proxy = Proxy

instance Group a => Group (Const a b) where invert (Const a) = Const (invert a)

instance Group b => Group (a -> b) where invert = (.) invert

instance Group (Sum Integer) where invert (Sum a) = Sum (Base.negate a)
instance Group (Sum Int) where invert (Sum a) = Sum (Base.negate a)
instance Group (Sum Word) where invert (Sum a) = Sum (Base.negate a)

newtype All a = All { getAll :: a }
  deriving (Eq, Bits, Base.Read, Base.Show)

newtype Any a = Any { getAny :: a }
  deriving (Eq, Bits, Base.Read, Base.Show)

newtype Xor a = Xor { getXor :: a }
  deriving (Eq, Bits, Base.Read, Base.Show)

instance Bits a => Semigroup (All a) where (<>) = (.&.)
instance Bits a => Semigroup (Any a) where (<>) = (.|.)
instance Bits a => Semigroup (Xor a) where (<>) = xor

instance Bits a => Abelian (All a)
instance Bits a => Abelian (Any a)
instance Bits a => Abelian (Xor a)

instance Bits a => Idempotent (All a)
instance Bits a => Idempotent (Any a)

instance Bits a => Monoid (All a) where
    mappend = (<>)
    mempty = All (complement zeroBits)

instance Bits a => Monoid (Any a) where
    mappend = (<>)
    mempty = Any zeroBits

instance Bits a => Monoid (Xor a) where
    mappend = (<>)
    mempty = Xor zeroBits

instance Bits a => Group (Xor a) where invert = Xor . complement . getXor

(+) :: Semigroup (Sum a) => a -> a -> a
a + b = getSum (Sum a <> Sum b)

(-) :: (Semigroup (Sum a), Group (Sum a)) => a -> a -> a
a - b = getSum (Sum a <> invert (Sum b))

(*) :: Semigroup (Product a) => a -> a -> a
a * b = getProduct (Product a <> Product b)

(/) :: (Semigroup (Product a), Group (Product a)) => a -> a -> a
a / b = getProduct (Product a <> invert (Product b))
