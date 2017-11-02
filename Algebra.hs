{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Algebra where

import Data.Monoid
import Data.Ord (Ord (..), Ordering (..))
import Data.Semigroup
import Numeric.Natural
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

infixr 7 <|
class (Semirng r, Semigroup a) => LeftModule r a where
    (<|) :: r -> a -> a

infixl 7 |>
class (Semirng r, Semigroup a) => RightModule r a where
    (|>) :: a -> r -> a

instance Monoid a => LeftModule Natural a where
    0 <| _ = mempty
    n <| a = a <> (n Base.- 1) <| a

instance Monoid a => RightModule Natural a where
    _ |> 0 = mempty
    a |> n = a |> (n Base.- 1) <> a

instance Group a => LeftModule Integer a where
    n <| a = case 0 `compare` n of EQ -> mempty
                                   LT -> invert a <> (n+1) <| a
                                   GT ->        a <> (n-1) <| a

instance Group a => RightModule Integer a where
    a |> n = case 0 `compare` n of EQ -> mempty
                                   LT -> a |> (n+1) <> invert a
                                   GT -> a |> (n-1) <>        a

type Module r a = (LeftModule r a, RightModule r a)

class (Monoid (RingSum a), Semigroup (RingProduct a)) => Semirng a where
    type RingSum a
    type RingProduct a

type Semiring a = (Semirng a, Monoid (RingProduct a))
type Rng a = (Semirng a, Group (RingSum a))
type Ring a = (Semiring a, Rng a)

instance Semirng Natural where
    type RingSum Natural = Sum Natural
    type RingProduct Natural = Product Natural

instance Semirng Integer where
    type RingSum Integer = Sum Integer
    type RingProduct Integer = Product Integer

instance Group (Sum Integer) where invert (Sum a) = Sum (Base.negate a)

(+) :: Semigroup (Sum a) => a -> a -> a
a + b = getSum (Sum a <> Sum b)

(-) :: Group (Sum a) => a -> a -> a
a - b = getSum (Sum a <> invert (Sum b))

(*) :: Semigroup (Product a) => a -> a -> a
a * b = getProduct (Product a <> Product b)

(/) :: Group (Product a) => a -> a -> a
a / b = getProduct (Product a <> invert (Product b))
