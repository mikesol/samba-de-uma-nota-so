module SambaDeUmaNotaSo.IO.SeventhVideo where

import Data.Maybe (Maybe)
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, Point)
import SambaDeUmaNotaSo.Types (VideoSpan, Windows)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

td2pt :: TouchedDot -> Point
td2pt TDOne = { x: 0.125, y: 0.125 }

td2pt TDTwo = { x: 0.375, y: 0.125 }

td2pt TDThree = { x: 0.625, y: 0.125 }

td2pt TDFour = { x: 0.875, y: 0.125 }

td2pt TDFive = { x: 0.125, y: 0.375 }

td2pt TDSix = { x: 0.375, y: 0.375 }

td2pt TDSeven = { x: 0.625, y: 0.375 }

td2pt TDEight = { x: 0.875, y: 0.375 }

td2pt TDNine = { x: 0.125, y: 0.625 }

td2pt TDTen = { x: 0.375, y: 0.625 }

td2pt TDEleven = { x: 0.625, y: 0.625 }

td2pt TDTwelve = { x: 0.875, y: 0.625 }

td2pt TDThirteen = { x: 0.125, y: 0.875 }

td2pt TDFourteen = { x: 0.375, y: 0.875 }

td2pt TDFifteen = { x: 0.625, y: 0.875 }

td2pt TDSixteen = { x: 0.875, y: 0.875 }

data TouchedDot
  = TDOne
  | TDTwo
  | TDThree
  | TDFour
  | TDFive
  | TDSix
  | TDSeven
  | TDEight
  | TDNine
  | TDTen
  | TDEleven
  | TDTwelve
  | TDThirteen
  | TDFourteen
  | TDFifteen
  | TDSixteen

type Accumulator
  = { videoSpan :: VideoSpan
    , deTodaAEscala :: NonEmptyToCofree DOMRect Painting
    , seventhVideoLoop :: NonEmptyToCofree (Windows Rectangle) Painting
    , dotMover ::
        NonEmptyToCofree DOMRect
          (Maybe Point -> { isTouched :: Boolean, dot :: Painting, touchedDot :: TouchedDot })
    }
