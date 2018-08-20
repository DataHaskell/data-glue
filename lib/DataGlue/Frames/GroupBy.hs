
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
  ) where

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
mean = (L.fold L.mean .) . (<$>) . view
