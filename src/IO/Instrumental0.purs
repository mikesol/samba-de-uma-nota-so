module SambaDeUmaNotaSo.IO.Instrumental0 where

import Data.List (List)
import Data.Typelevel.Num (D12)
import Data.Vec as V
import Graphics.Painting (Painting, Point)
import SambaDeUmaNotaSo.Types (VideoSpan)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

type Ctxt'
  = ( canvas :: DOMRect
    , halfW :: Number
    , halfH :: Number
    , mwh :: Number
    , asdr :: Number -> Number
    , startsAt :: Number
    , translations :: Instrumental0 Point
    , activeZones :: Instrumental0 (List Number)
    )

type Ctxt
  = { time :: Number
    , timeDiff :: Number
    , timeDiffQuantizedToHalfBeat :: Number
    , colors :: Instrumental0 FauxColor
    , startsAt :: Number
    | Ctxt'
    }

type FauxColor
  = { r :: Int, g :: Int, b :: Int, a :: Number }

type Instrumental0 a
  = { wedges :: V.Vec D12 a
    , center :: a
    , ring0 :: a
    , ring1 :: a
    , background :: a
    }

type Accumulator
  = { videoSpan :: VideoSpan
    , activeZones :: Instrumental0 (List Number)
    , instruments :: NonEmptyToCofree { | Ctxt' } Painting
    }
