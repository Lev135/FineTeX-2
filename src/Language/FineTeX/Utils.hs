{-# LANGUAGE DeriveFunctor #-}
module Language.FineTeX.Utils where
import Data.Bifunctor

data Info i a = I
  { getInfo :: i
  , getVal  :: a
  }
  deriving (Eq, Functor, Ord, Show)

instance Bifunctor Info where
  bimap f g (I i a) = I (f i) (g a)

mkI :: Monoid i => a -> Info i a
mkI = I mempty


todo :: a
todo = error "todo"
{-# WARNING todo "todo" #-}
