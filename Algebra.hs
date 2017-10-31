{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Algebra where

import Control.Category
import Data.Functor
import Data.Functor.Const
import Data.Functor.Identity
import Data.Monoid hiding ((<>))
import Data.Proxy
import Data.Semigroup
import Prelude (Integer)
import qualified Prelude as Base

class Semigroup a => Abelian a

class Semigroup a => Idempotent a

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

instance Group a => Group (Base.IO a) where invert = fmap invert

instance Group (Sum Integer) where invert (Sum a) = Sum (Base.negate a)

(+) :: Semigroup (Sum a) => a -> a -> a
a + b = getSum (Sum a <> Sum b)

(-) :: (Semigroup (Sum a), Group (Sum a)) => a -> a -> a
a - b = getSum (Sum a <> invert (Sum b))

(*) :: Semigroup (Product a) => a -> a -> a
a * b = getProduct (Product a <> Product b)

(/) :: (Semigroup (Product a), Group (Product a)) => a -> a -> a
a / b = getProduct (Product a <> invert (Product b))
