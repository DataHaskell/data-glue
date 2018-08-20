
{-| This modules provides top-level functions upon the @Frames@ module to
    perform simple group-by operations.

    Import the module qualified to avoid clashing with the Prelude:

>>> import qualified DataGlue.Frames.GroupBy as G

    Example of group-by operation:

>>> G.groupByOp colA df G.mean [colB, colC, colD]

-}
module DataGlue.Frames.GroupBy
  ( groupByOp
  , mean
  , product
  , std
  , sum
  , variance
  ) where

import Prelude hiding (product, sum)
import qualified Control.Foldl as L
import Control.Lens (Getting, view)
import DataGlue.Frames
import Frames.InCore (RecVec)

-- | Given a lens, split a 'Frame' occording the different values of the related
--   field.
groups :: (RecVec rs, Eq a, Ord a)
  => Getting a (Record rs) a
  -> FrameRec rs
  -> [FrameRec rs]
groups col df = flip filterFrame df . isClass <$> uniques col df
  where isClass value row = view col row == value

-- | Group-by operation: according one columns, the dataframe is reduced to
--   unique values of this column, and the other columns values are the result
--   of a given operation.
groupByOp
  :: (Functor f, RecVec rs, Eq s, Ord s)
  => Getting s (Record rs) s --  ^ Lens of the column to group by
  -> FrameRec rs -- ^ a dataframe
  -> (a -> FrameRec rs -> b) -- the operation to apply to the other columns
  -> f a -> f [b] -- ^ the columns where apply the operation
groupByOp col df operation columns = (<$> groups col df) . operation <$> columns

-- | Get the mean of a column, using a 'Lens'.
mean :: (Functor f, Fractional c, Foldable f) => Getting c s c -> f s -> c
mean = gOp L.mean

-- | Get the product of a column, using a 'Lens'.
product :: (Functor f, Num c, Foldable f) => Getting c s c -> f s -> c
product = gOp L.product

-- | Get the std of a column, using a 'Lens'.
std :: (Functor f, Floating c, Foldable f) => Getting c s c -> f s -> c
std = gOp L.std

-- | Get the sum of a column, using a 'Lens'.
sum :: (Functor f, Num c, Foldable f) => Getting c s c -> f s -> c
sum = gOp L.sum

-- | Get the variance of a column, using a 'Lens'.
variance :: (Functor f, Fractional c, Foldable f) => Getting c s c -> f s -> c
variance = gOp L.variance

gOp :: (Foldable f, Functor f) => L.Fold a c -> Getting a s a -> f s -> c
gOp f = (L.fold f .) . (<$>) . view
