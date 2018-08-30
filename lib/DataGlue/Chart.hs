{-| This module provides top-level functions upon the @Chart@ module to
    simplify data science visualisation in a iHaskell notebook.

    Currently, this module only contains reimports.
-}

module DataGlue.Chart
  ( def
  , module Data.Colour
  , module Graphics.Rendering.Chart
  ) where

import Data.Colour
import Data.Default.Class (def)
import Graphics.Rendering.Chart
