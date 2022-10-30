{-# LANGUAGE DeriveFunctor #-}
module Language.FineTeX.Utils where
import Data.Bifunctor

data Info i a = I
  { getInfo :: i
  , getVal  :: a
  }
  deriving (Eq, Functor, Ord)

instance (Show i, Show a) => Show (Info i a) where
  show (I i a)
    | show i == "()" = show a
    | otherwise =  "I " ++ show i ++ " " ++ show a

instance Bifunctor Info where
  bimap f g (I i a) = I (f i) (g a)

mkI :: Monoid i => a -> Info i a
mkI = I mempty


todo :: a
todo = error "todo"
{-# WARNING todo "todo" #-}
