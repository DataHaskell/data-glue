{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module DataGlue.Frames
  ( dropFrameRow
  , takeFrameRow
  , module Frames
  ) where

import Data.Foldable
import qualified Data.List as L
import Data.Vinyl.Functor (Identity)
import Frames
import Frames.InCore (RecVec)
import qualified IHaskell.Display as D

takeFrameRow :: Int -> Frame r -> Frame r
takeFrameRow n (Frame fLen fRow) = Frame (min n fLen) fRow

dropFrameRow :: Int -> Frame r -> Frame r
dropFrameRow n (Frame fLen fRow) = Frame (max 0 (fLen - n)) (\i -> fRow (i + n))

type FRecord ts = (AsVinyl ts, ColumnHeaders ts, RecAll Identity ts Show
    , RecAll Identity (UnColumn ts) Show, RecVec ts)

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

prettyRecFrame :: FRecord ts => Frame (Record ts) -> D.Display
prettyRecFrame df =
    D.Display [D.html $ prettyTable $ (prettyHRow headers) ++ prettyFrame]
  where
    headers = columnHeaders df
    prettyFrame = sampledFrame df f (L.length $ columnHeaders df)
    f = foldMap (prettyRow . showFields)

prettyValueFrame :: Show a => Frame a -> D.Display
prettyValueFrame df = D.Display [D.html $ prettyTable $ prettyFrame]
  where
    prettyFrame = sampledFrame df f 1
    f = foldMap (prettyRow . return . show)

sampledFrame :: Frame r -> (Frame r -> String) -> Int -> String
sampledFrame df@(Frame fLen _) f width
    | fLen < 20 = f df
    | otherwise = f (takeFrameRow 10 df)
        ++ prettySampleSeparator width
        ++ f (dropFrameRow (fLen-10) df)

prettyRecord :: FRecord ts => Record ts -> D.Display
prettyRecord rec = prettyRecFrame $ toFrame [rec]

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

-- XXX: Do `describe` (like in Python)
-- XXX: Do `str` (like in R)
