module Algebra (Semigroup (..), Monoid (mempty), Group (..), Abelian, Idempotent,
                (+), (-), (*), (/), (×), commuteWith) where

import Control.Category
import qualified Control.Category.Dual as C
import Data.Functor
import Data.Functor.Const
import Data.Functor.Identity
import Data.Monoid hiding ((<>))
import Data.Proxy
import Data.Semigroup
import Data.Word
import Numeric.Natural
import Prelude (Int, Integer)
import qualified Prelude as Base
import qualified Data.Ratio as Base

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

instance Abelian All
instance Abelian Any

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

deriving instance Group (k b a) => Group (C.Dual k a b)

instance Base.Num a => Group (Sum a) where invert (Sum a) = Sum (Base.negate a)
instance Base.Fractional a => Group (Product a) where invert (Product a) = Product (Base.recip a)

instance Base.Integral a => Group (Product (Base.Ratio a)) where invert = fmap Base.recip

infixl 6 +, -
infixl 7 ×, *, /

(+) :: Semigroup (Sum a) => a -> a -> a
a + b = getSum (Sum a <> Sum b)

(-) :: Group (Sum a) => a -> a -> a
a - b = getSum (Sum a <> invert (Sum b))

(*) :: Semigroup (Product a) => a -> a -> a
a * b = getProduct (Product a <> Product b)

(×) :: Semigroup (Product a) => a -> a -> a
a × b = getProduct (Product a <> Product b)

(/) :: (Semigroup (Product a), Group (Product a)) => a -> a -> a
a / b = getProduct (Product a <> invert (Product b))

commuteWith :: Group b => (a -> a -> b) -> a -> a -> b
commuteWith f x y = f x y <> invert (f y x)
