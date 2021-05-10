module SambaDeUmaNotaSo.IO.Instrumental1 where

import Prelude
import Data.List (List)
import Data.Typelevel.Num (D7)
import Data.Vec as V
import Graphics.Painting (Painting, Point)
import SambaDeUmaNotaSo.Types (VideoSpan)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)
import SambaDeUmaNotaSo.IO.InstrumentalShared as IS

type Ctxt'
  = IS.Ctxt' Instrumental1

type Ctxt
  = IS.Ctxt Instrumental1

type FauxColor
  = IS.FauxColor

type Instrumental1 a
  = { boxes :: V.Vec D7 a
    , ball :: a
    }

type Accumulator
  = { videoSpan :: VideoSpan
    , activeZones :: Instrumental1 (List Number)
    , instruments :: NonEmptyToCofree { | Ctxt' } Painting
    }
