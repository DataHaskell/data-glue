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
  display = return . prettyFrame

instance FRecord ts => D.IHaskellDisplay (Record ts) where
  display = return . prettyRecord

prettyFrame :: FRecord ts => Frame (Record ts) -> D.Display
prettyFrame df@(Frame fLen _) =
    D.Display [D.html $ prettyTable headers prettydf]
  where
    headers = columnHeaders df
    prettydf
      | fLen < 20 = prettyPartofTable df
      | otherwise = prettyPartofTable (takeFrameRow 10 df)
          ++ "<tr><td style='text-align:center' colspan="
          ++ (show $ L.length headers) ++">. . .</td></tr>"
          ++ prettyPartofTable (dropFrameRow (fLen-10) df)
    prettyTable = (("<table>" ++) .) . (. (++ "</table>")) . (++) . prettyHRow
    prettyPartofTable = foldMap prettyRow
    prettyRow = (++ "</tr>") . foldl' ((. prettyCell) . (++)) "<tr>"
        . showFields
    prettyCell = ("<td>" ++) . (++ "</td>")
    prettyHRow = (++ "</tr>") . foldl' ((. prettyHCell) . (++)) "<tr>"
    prettyHCell = ("<th>" ++) . (++ "</th>")

prettyRecord :: FRecord ts => Record ts -> D.Display
prettyRecord rec = prettyFrame $ toFrame [rec]

-- XXX: Do `describe` (like in Python)
-- XXX: Do `str` (like in R)
