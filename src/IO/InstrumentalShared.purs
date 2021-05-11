module SambaDeUmaNotaSo.IO.InstrumentalShared where

import Prelude
import Data.List (List)
import Data.Typelevel.Num (D12)
import Data.Vec as V
import Graphics.Painting (Painting, Point)
import Web.HTML.HTMLElement (DOMRect)

type FauxColor
  = { r :: Int, g :: Int, b :: Int, a :: Number }
