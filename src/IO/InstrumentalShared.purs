module SambaDeUmaNotaSo.IO.InstrumentalShared where

import Prelude
import Data.List (List)
import Data.Typelevel.Num (D12)
import Data.Vec as V
import Graphics.Painting (Painting, Point)
import Web.HTML.HTMLElement (DOMRect)

type Ctxt' f
  = ( canvas :: DOMRect
    , halfW :: Number
    , halfH :: Number
    , mwh :: Number
    , asdr :: Number -> Number
    , startsAt :: Number
    , translations :: f Point
    , activeZones :: f (List Number)
    )

type Ctxt f
  = { time :: Number
    , timeDiff :: Number
    , timeDiffQuantizedToHalfBeat :: Number
    , colors :: f  FauxColor
    , startsAt :: Number
    | Ctxt' f
    }

type FauxColor
  = { r :: Int, g :: Int, b :: Int, a :: Number }
