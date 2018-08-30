{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-| This modules provides top-level functions upon the @Frames@ module to
    simplify data science operations in a iHaskell notebook.
-}

module DataGlue.Frames
  ( describe
  , dropColumn
  , dropColumns
  , dropField
  , dropFields
  , dropFrameRow
  , splitFrame
  , takeFrameRow
  , uniques
  , values
  , module Frames
  ) where

import qualified Control.Foldl as L
import Control.Lens (Getting, view)
import Data.Foldable
import qualified Data.List as LI
import Data.Typeable
import Data.Vinyl.Functor (Identity)
import Data.Vinyl.TypeLevel (RImage)
import Frames
import Frames.InCore (RecVec)
import qualified IHaskell.Display as D
import System.Random (StdGen, mkStdGen, randoms)

-- | Returns the input 'Frame' restricted to its @n@ first rows.
takeFrameRow :: Int -> Frame r -> Frame r
takeFrameRow n (Frame fLen fRow) = Frame (min n fLen) fRow

-- | Returns the input 'Frame' omitting its @n@ first rows.
dropFrameRow :: Int -> Frame r -> Frame r
dropFrameRow n (Frame fLen fRow) = Frame (max 0 (fLen - n)) (\i -> fRow (i + n))

-- | Returns the type of the columns.
colTypes :: Typeable r => Frame r -> [String]
colTypes = go . f . l . head . f . typeOf
  where
      go [] = []
      go [x] = [show (l x)]
      go (x:xs:_) = show (l x) : go (f xs)
      f = snd . splitTyConApp
      l = last . f

-- | Returns names and types of the columns.
colmanifest
  :: (Typeable cs, Typeable f, ColumnHeaders cs)
  => Frame (Rec f cs) -> [(String, String)]
colmanifest df = zip (columnHeaders df) (colTypes df)

-- | Returns a description of the Frame dimensions.
dimensions :: (ColumnHeaders cs) => Frame (Rec f cs) -> String
dimensions df = height ++ "x" ++ width ++ " dataframe."
  where
    height = show $ length df
    width = show . LI.length $ columnHeaders df

-- | Describe the input Frame. Currently limited to its dimensions.
describe
  :: (Typeable cs, Typeable f, ColumnHeaders cs)
  => Frame (Rec f cs) -> D.Display
describe df = D.Display [D.plain $ dim ++ cols]
  where
    dim = dimensions df ++ "\n"
    cols = LI.intercalate "\n" $
             (\(n,t) -> "  column \"" ++ n ++ "\": " ++ t) <$> colmanifest df

-- | 'values' @col@ @frame@ returns all the values from the 'Frame' @f@,
--   given the 'Lens' @col@.
values :: Functor f => Getting b s b -> f s -> f b
values = (<$>) . view

-- | 'uniques' has the same prupose than 'values', but with duplicated removed.
uniques :: (Foldable f, Ord a, Functor f) => Getting a s a -> f s -> [a]
uniques = (L.fold L.nub .) . values

-- | Randomly splits a 'Frame', to get train and test sets, given a portion.
splitFrame
  :: RealFrac a
  => Int -- ^ a random seed
  -> Frame r -- ^ a frame
  -> a -- ^ proportion of the split.
  -> (Frame r, Frame r)
splitFrame seed df n =
  let shuffled_indexes = shuffle (mkStdGen seed) [0..length df-1]
      shuffled = df {frameRow = \i -> frameRow df (shuffled_indexes!!i)}
      size = floor $ fromIntegral (length df) * n
  in (takeFrameRow size shuffled, dropFrameRow size shuffled)

type family Drop t l where
  Drop t '[] = '[]
  Drop t (t ': xs) = xs -- We only drop the first one
  Drop t (t' ': xs) = t' ': Drop t xs

type family Drops t l where
  Drops '[] l = l
  Drops (t ': ts) l = Drops ts (Drop t l)

-- | Drops a field of a 'Record'.
dropField
  :: forall t xs. (RecSubset Rec (Drop t xs) xs (RImage (Drop t xs) xs))
  => Record xs -> Record (Drop t xs)
dropField = rcast

-- | Drops multiple fields of a 'Record'.
dropFields
  :: forall ts xs. (RecSubset Rec (Drops ts xs) xs (RImage (Drops ts xs) xs))
  => Record xs -> Record (Drops ts xs)
dropFields = rcast

-- | Convenient fonction to map 'dropField' over a 'Frame'.
dropColumn
  :: forall t xs. (RecSubset Rec (Drop t xs) xs (RImage (Drop t xs) xs))
  => Frame (Record xs) -> Frame (Record (Drop t xs))
dropColumn = (dropField @t <$>)

-- | Convenient fonction to map 'dropFields' over a 'Frame'.
dropColumns
  :: forall t xs. (RecSubset Rec (Drops t xs) xs (RImage (Drops t xs) xs))
  => Frame (Record xs) -> Frame (Record (Drops t xs))
dropColumns = (dropFields @t <$>)

-- | Shuffles a list, given a 'StdGen'.
shuffle :: StdGen -> [a] -> [a]
shuffle = shuffle' . randoms
  where
    shuffle' _ [] = []
    shuffle' (i:is) xs = let (left, a:right) = splitAt (i `mod` length xs) xs
                         in a : shuffle' is (left ++ right)
    shuffle' [] _ = error "No indexes provided." -- Can't happen (exhaustivity)


type FRecord ts = (AsVinyl ts, ColumnHeaders ts, RecAll Identity ts Show
    , RecAll Identity (UnColumn ts) Show, RecVec ts)

{-| Instances for easy prettyprint of Frames -}
instance FRecord ts => D.IHaskellDisplay (Frame (Record ts)) where
  display = return . prettyRecFrame

instance D.IHaskellDisplay (Frame Int) where
  display = return . prettyValueFrame

instance D.IHaskellDisplay (Frame Double) where
  display = return . prettyValueFrame

instance D.IHaskellDisplay (Frame Text) where
  display = return . prettyValueFrame

instance FRecord ts => D.IHaskellDisplay (Record ts) where
  display = return . prettyRecord

-- | Rendering of a 'Frame' of 'Record' as a table.
prettyRecFrame :: FRecord ts => Frame (Record ts) -> D.Display
prettyRecFrame df =
    D.Display [D.html $ prettyTable $ (prettyHRow headers) ++ prettyFrame]
  where
    headers = columnHeaders df
    prettyFrame = sampledFrame df f (LI.length $ columnHeaders df)
    f = foldMap (prettyRow . showFields)

-- | Rendering of a 'Frame' of showable values as a table.
prettyValueFrame :: Show a => Frame a -> D.Display
prettyValueFrame df = D.Display [D.html $ prettyTable $ prettyFrame]
  where
    prettyFrame = sampledFrame df f 1
    f = foldMap (prettyRow . return . show)

-- | Limits the rendering of a 'Frame' to the first 10 and last 10 rows.
sampledFrame :: Frame r -> (Frame r -> String) -> Int -> String
sampledFrame df@(Frame fLen _) f width
    | fLen < 20 = f df
    | otherwise = f (takeFrameRow 10 df)
        ++ prettySampleSeparator width
        ++ f (dropFrameRow (fLen-10) df)

-- | Rendering of a 'Record'.
prettyRecord :: FRecord ts => Record ts -> D.Display
prettyRecord rec = prettyRecFrame $ toFrame [rec]

-- | Rendering of a separator in an HTML table.
prettySampleSeparator :: (Num a, Show a) => a -> String
prettySampleSeparator n =
  "<tr><td style='text-align:center' colspan=" ++ show n ++">. . .</td></tr>"

prettyRow, prettyHRow :: [String] -> String
prettyRow = prettyRow' prettyCell
prettyHRow = prettyRow' prettyHCell

prettyRow' :: (String -> String) -> [String] -> String
prettyRow' f = innerHtml "tr" . foldl' ((. f) . (++)) ""

prettyTable, prettyCell, prettyHCell :: String -> String
prettyTable = innerHtml "table"
prettyCell = innerHtml "td"
prettyHCell = innerHtml "th"

innerHtml :: String -> String -> String
innerHtml tag = (inHtml tag ++) . (++ outHtml tag)

inHtml, outHtml :: String -> String
inHtml = ("<" ++) . (++ ">")
outHtml = ("</" ++) . (++ ">")

-- XXX: Do `str` (like in R)
