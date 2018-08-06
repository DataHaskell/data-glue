{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DataGlue.Base
  ( dropFrameRow
  , prettyFrame
  , takeFrameRow
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

prettyFrame
  :: (AsVinyl ts, RecAll Identity ts Show, RecAll Identity (UnColumn ts) Show)
  => Frame (Record ts) -> D.Display
prettyFrame df@(Frame fLen _) =
    D.Display [D.html $ "<table>" ++ prettyHeaders ++ prettydf ++ "</table>"]
  where
    prettyHeaders = prettyHRow getHeaders
    getHeaders -- XXX: Do a better implementation by reading vinyl Rec.
      | fLen > 0 = map (T.strip . snd . T.breakOnEnd ", ")
          (init . T.splitOn ":->" . T.pack . tail . show $ frameRow df 0)
      | otherwise = []
    prettydf
      | fLen < 20 = prettyTable df
      | otherwise = prettyTable (takeFrameRow 10 df)
          ++ "<tr><td style='text-align:center' colspan=999>. . .</td></tr>"
          ++ prettyTable (dropFrameRow (fLen-10) df)
    prettyTable = foldMap prettyRow
    prettyRow = (++ "</tr>") . foldl' ((. prettyCell) . (++)) "<tr>"
      . showFields
    prettyHRow = (++ "</tr>") . foldl' ((. prettyHCell) . (++)) "<tr>"
    prettyCell = ("<td>" ++) . (++ "</td>")
    prettyHCell = ("<th>" ++) . (++ "</th>") . T.unpack
