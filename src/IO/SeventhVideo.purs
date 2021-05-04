module SambaDeUmaNotaSo.IO.SeventhVideo where

import Data.Maybe (Maybe)
import Data.Typelevel.Num (D6)
import Data.Vec as V
import Data.Vec ((+>))
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

-- >>> a='One Two Three Four Five Six Seven Eight Nine Ten Eleven Twelve Thirteen Fourteen Fifteen Sixteen'
-- >>> a=a.split(' ')
-- >>> for i, x in enumerate(a): print('td2harmChain TD%s = %s' % (x, (' +> '.join(['TD%s' % a[(i + y * 3) % 16] for y in range(1,7)]))+' +> V.empty'))
td2harmChain :: TouchedDot -> V.Vec D6 TouchedDot
td2harmChain TDOne = TDFour +> TDSeven +> TDTen +> TDThirteen +> TDSixteen +> TDThree +> V.empty

td2harmChain TDTwo = TDFive +> TDEight +> TDEleven +> TDFourteen +> TDOne +> TDFour +> V.empty

td2harmChain TDThree = TDSix +> TDNine +> TDTwelve +> TDFifteen +> TDTwo +> TDFive +> V.empty

td2harmChain TDFour = TDSeven +> TDTen +> TDThirteen +> TDSixteen +> TDThree +> TDSix +> V.empty

td2harmChain TDFive = TDEight +> TDEleven +> TDFourteen +> TDOne +> TDFour +> TDSeven +> V.empty

td2harmChain TDSix = TDNine +> TDTwelve +> TDFifteen +> TDTwo +> TDFive +> TDEight +> V.empty

td2harmChain TDSeven = TDTen +> TDThirteen +> TDSixteen +> TDThree +> TDSix +> TDNine +> V.empty

td2harmChain TDEight = TDEleven +> TDFourteen +> TDOne +> TDFour +> TDSeven +> TDTen +> V.empty

td2harmChain TDNine = TDTwelve +> TDFifteen +> TDTwo +> TDFive +> TDEight +> TDEleven +> V.empty

td2harmChain TDTen = TDThirteen +> TDSixteen +> TDThree +> TDSix +> TDNine +> TDTwelve +> V.empty

td2harmChain TDEleven = TDFourteen +> TDOne +> TDFour +> TDSeven +> TDTen +> TDThirteen +> V.empty

td2harmChain TDTwelve = TDFifteen +> TDTwo +> TDFive +> TDEight +> TDEleven +> TDFourteen +> V.empty

td2harmChain TDThirteen = TDSixteen +> TDThree +> TDSix +> TDNine +> TDTwelve +> TDFifteen +> V.empty

td2harmChain TDFourteen = TDOne +> TDFour +> TDSeven +> TDTen +> TDThirteen +> TDSixteen +> V.empty

td2harmChain TDFifteen = TDTwo +> TDFive +> TDEight +> TDEleven +> TDFourteen +> TDOne +> V.empty

td2harmChain TDSixteen = TDThree +> TDSix +> TDNine +> TDTwelve +> TDFifteen +> TDTwo +> V.empty

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
