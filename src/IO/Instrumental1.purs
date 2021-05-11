module SambaDeUmaNotaSo.IO.Instrumental1 where

import Prelude
import Data.Maybe (Maybe)
import Data.Typelevel.Num (D5)
import Data.Vec as V
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.IO.InstrumentalShared as IS
import SambaDeUmaNotaSo.Types (VideoSpan, Windows)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

type Ctxt'
  = ( canvas :: DOMRect
    , eighthW :: Number
    , eighthH :: Number
    , boxY :: Number
    , mwh :: Number
    , onOff :: Instrumental1 Boolean
    )

type Ctxt
  = { time :: Number
    , startsAt :: Number
    , timePassed :: Number
    , colors :: Instrumental1 FauxColor
    , ballPos :: Number
    | Ctxt'
    }

type FauxColor
  = IS.FauxColor

type Instrumental1 a
  = { boxes :: V.Vec D5 a
    , ball :: a
    }

mapInstrumental1 :: forall a b. (a -> b) -> Instrumental1 a -> Instrumental1 b
mapInstrumental1 f { boxes, ball } = { boxes: map f boxes, ball: f ball }

type Accumulator
  = { videoSpan :: VideoSpan
    , onOff :: Instrumental1 Boolean
    , instruments :: NonEmptyToCofree { | Ctxt' } Painting
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    }
