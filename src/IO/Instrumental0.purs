module SambaDeUmaNotaSo.IO.Instrumental0 where

import Prelude
import Data.List (List)
import Data.Typelevel.Num (D12)
import Data.Vec as V
import Graphics.Painting (Painting, Point)
import SambaDeUmaNotaSo.Types (VideoSpan)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)
import SambaDeUmaNotaSo.IO.InstrumentalShared as IS

type Ctxt'
  = IS.Ctxt' Instrumental0

type Ctxt
  = IS.Ctxt Instrumental0

type FauxColor
  = IS.FauxColor

mapInstrumental0 :: forall a b. (a -> b) -> Instrumental0 a -> Instrumental0 b
mapInstrumental0 f { wedges, center, ring0, ring1, background } = { wedges: map f wedges, center: f center, ring0: f ring0, ring1: f ring1, background: f background }

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
