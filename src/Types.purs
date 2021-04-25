module SambaDeUmaNotaSo.Types where

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Typelevel.Num (D7)
import Data.Vec (Vec)
import Graphics.Painting (Painting, Point)
import Web.HTML.HTMLElement (DOMRect)

type RGB
  = { r :: Int, g :: Int, b :: Int }

type Windows a
  = Vec D7 a

type Interactions
  = List Point

type BaseEnv
  = { time :: Number
    , interaction :: Maybe Point
    , canvas ::DOMRect
    }

type AugmentedEnv
  = { time :: Number
    , interaction :: Maybe Point
    , canvas :: DOMRect
    , background :: Painting
    }

type FirstPartEnv
  = { time :: Number
    , interaction :: Maybe Point
    , canvas :: DOMRect
    , background :: Painting
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , isWindowTouched :: Windows Boolean
    }

type VideoSpan
  = { start :: Number, end :: Number }
