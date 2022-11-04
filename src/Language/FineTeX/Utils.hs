{-# LANGUAGE DeriveFunctor #-}
module Language.FineTeX.Utils where
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Writer (MonadWriter(tell), WriterT(runWriterT))
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = fmap f . g

todo :: a
todo = error "todo"
{-# WARNING todo "todo" #-}

newtype SourceId
  = SourceId Int
  deriving (Eq, Ord, Show)

data Pos = Pos
  { -- | Source file id
    posSource :: SourceId
    -- | Range of source offsets, where element is situated
  , posRange  :: (Int, Int)
  }
  deriving (Eq, Ord, Show)

data Posed a = Posed
  { getPos :: Maybe Pos
  , getVal :: a
  }
  deriving (Functor, Generic)

instance Semigroup a => Semigroup (Posed a) where
  x <> y = firstLastPos x y (getVal x <> getVal y)

instance Monoid a => Monoid (Posed a) where
  mempty = nopos mempty

instance IsString s => IsString (Posed s) where
  fromString = nopos . fromString
nopos :: a -> Posed a
nopos = Posed Nothing

pos :: Pos -> a -> Posed a
pos p = Posed (Just p)

posFrom :: a -> Posed b -> Posed a
posFrom a (Posed p _) = Posed p a

firstLastPos :: Posed a -> Posed b -> c -> Posed c
firstLastPos (Posed p _) (Posed p' _) = Posed pos
  where pos = case (p, p') of
          (Nothing, _) -> p'
          (_, Nothing) -> p
          (Just (Pos src (b, _)), Just (Pos _ (_, e)))
                       -> Just $ Pos src (b, e)

sequencePos :: [Posed a] -> Posed [a]
sequencePos [] = nopos []
sequencePos xs = firstLastPos (head xs) (last xs) (getVal <$> xs)


type ErrorT e m = WriterT [e] m

tellErr :: MonadWriter [e] m => e -> m ()
tellErr = tell . (:[])

runErrorM :: MonadError [e] m => ErrorT e m a -> m a
runErrorM ma = do
  (a, es) <- runWriterT ma
  if null es
    then pure a
    else throwError es

instance Eq a => Eq (Posed a) where
  (==) = (==) `on` getVal
instance Ord a => Ord (Posed a) where
  compare = compare `on` getVal
instance Show a => Show (Posed a) where
  show = show . getVal

type ErrDoc = P.Doc Void

class PrettyErr e where
  prettyErr :: e -> [Posed ErrDoc]

-- | File source code split by eols
data Src = Src
  { srcPath  :: FilePath
  , srcLines :: [Text]
  }
  deriving (Eq, Ord, Show)

type Srcs = Map SourceId Src

renderErrors :: PrettyErr e => Srcs -> [e] -> Text
renderErrors srcs = P.renderStrict . P.layoutSmart errRenderOpts
  . P.vsep . map renderErr
  where
    errRenderOpts = P.defaultLayoutOptions
      { P.layoutPageWidth = P.AvailablePerLine 80 1 }
    renderErr = P.vsep . map renderErrEl . prettyErr
    renderErrEl :: Posed ErrDoc -> ErrDoc
    renderErrEl (Posed mp doc)
      = fromMaybe doc do
          Pos srcId p <- mp
          Src path ls <- M.lookup srcId srcs
          (ln, cols, errLn) <- takeLine p ls
          pure $ P.vsep
            [ mconcat (P.punctuate ":"
                  [P.pretty path, P.pretty $ ln + 1, P.pretty $ fst cols + 1])
                <> ":" P.<+> doc
            , P.pretty errLn
            , P.pretty (mask cols)
            ]

    takeLine :: (Int, Int) -> [Text] -> Maybe (Int, (Int, Int), Text)
    takeLine (b, e) = go 0 0
      where
        go _ _ [] = Nothing
        go ln o (l : ls)
            | (b - o) < len = Just (ln, (b - o, (e - o) `min` (len - 1)), l)
            | otherwise     = go (succ ln) (o + len) ls
          where len = T.length l

    mask (b, e) = T.concat [ T.replicate b " "
                           , "^"
                           , T.replicate (e - b - 1) "~"
                           , if b < e then "^" else ""
                           ]
