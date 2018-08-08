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
import qualified IHaskell.Display as D
import qualified Data.Text as T

takeFrameRow :: Int -> Frame r -> Frame r
takeFrameRow n (Frame fLen fRow) = Frame (min n fLen) fRow

dropFrameRow :: Int -> Frame r -> Frame r
dropFrameRow n (Frame fLen fRow) = Frame (max 0 (fLen - n)) (\i -> fRow (i + n))

type FRecord ts =
    (AsVinyl ts, RecAll Identity ts Show, RecAll Identity (UnColumn ts) Show)

instance FRecord ts => D.IHaskellDisplay (Frame (Record ts)) where
  display = return . prettyFrame

instance FRecord ts => D.IHaskellDisplay (Record ts) where
  display = return . prettyRecord

prettyFrame :: FRecord ts => Frame (Record ts) -> D.Display
prettyFrame df@(Frame fLen _) =
    D.Display [D.html $ (prettyTable getHeaders prettydf) ++ dfMetrics]
  where
    getHeaders -- XXX: Do a better implementation by reading vinyl Rec.
      | fLen > 0 = extractHeadersFromRecord $ frameRow df 0
      | otherwise = []
    prettydf
      | fLen < 20 = prettyPartofTable df
      | otherwise = prettyPartofTable (takeFrameRow 10 df)
          ++ "<tr><td style='text-align:center' colspan="
          ++ (show nCols) ++">. . .</td></tr>"
          ++ prettyPartofTable (dropFrameRow (fLen-10) df)
    prettyPartofTable = foldMap prettyRow
    nCols = L.length getHeaders
    dfMetrics = show fLen ++ " x " ++ (show nCols) ++ " dataframe."

prettyRecord :: FRecord ts => Record ts -> D.Display
prettyRecord rec =
    D.Display [D.html $ prettyTable getHeaders $ prettyRow rec]
  where
    getHeaders = extractHeadersFromRecord rec

extractHeadersFromRecord :: FRecord ts => Record ts -> [String]
extractHeadersFromRecord = map (T.unpack . T.strip . snd . T.breakOnEnd ", ")
    . init . T.splitOn ":->" . T.pack . tail . show

prettyTable :: [String] -> String -> String
prettyTable = (("<table>" ++) .) . (. (++ "</table>")) . (++) . prettyHRow

prettyRow :: FRecord ts => Record ts -> String
prettyRow = (++ "</tr>") . foldl' ((. prettyCell) . (++)) "<tr>" . showFields
  where
    prettyCell = ("<td>" ++) . (++ "</td>")

prettyHRow :: [String] -> String
prettyHRow = (++ "</tr>") . foldl' ((. prettyHCell) . (++)) "<tr>"
  where
    prettyHCell = ("<th>" ++) . (++ "</th>")


