{-# LANGUAGE DeriveFunctor #-}
module Language.FineTeX.Utils where
import Data.Bifunctor
import Data.Function

data Info i a = I
  { getInfo :: i
  , getVal  :: a
  }
  deriving (Functor)

type WrapI i a = Info i (a i)

instance Show a => Show (Info i a) where
  show = show . getVal
instance Eq a => Eq (Info i a) where
  (==) = (==) `on` getVal
instance Ord a => Ord (Info i a) where
  compare = compare `on` getVal
instance Bifunctor Info where
  bimap f g (I i a) = I (f i) (g a)

mkI :: Monoid i => a -> Info i a
mkI = I mempty
