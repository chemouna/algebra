module Group where

import Data.Monoid

class Monoid m => Group m where
  invert :: m -> m

instance Group () where
  invert () = ()

instance Num a => Group (Sum a) where
  invert = Sum . negate . getSum

instance Fractional a => Group (Product a) where
  invert = Product . recip . getProduct

instance Group a => Group (Dual a) where
  invert = Dual . invert getDual

instance Group b => Group (a -> b) where
  invert f = invert . f

instance (Group a, Group b) => Group (a, b) where
  invert (a, b) = (invert a, invert b)

instance (Group a, Group b, Group c) => Group (a, b, c) where
  invert (a, b, c) = (invert a, invert b, invert c)

instance (Group a, Group b, Group c, Group d) => Group (a, b, c, d) where
  invert (a, b, c, d) = (invert a, invert b, invert c, invert d)

instance (Group a, Group b, Group c, Group d, Group e) => Group (a, b, c, d, e) where
  invert (a, b, c, d, e) = (invert a, invert b, invert c, invert d, invert e)

-- Abelian Groups
class Group g => Abelian g

instance Abelian ()

instance Num a => Abelian (Sum a)

instance Fractional a => Abelian (Product a)

instance Abelian a => Abelian (Dual a)

-- TODO: other examples

-- Cyclic group

