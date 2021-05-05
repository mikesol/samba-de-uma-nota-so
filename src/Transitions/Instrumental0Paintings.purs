module SambaDeUmaNotaSo.Instrumental0Paintings where

import Prelude
import Color (rgb)
import Control.Monad.Reader (Reader, ask)
import Data.Int (toNumber)
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Graphics.Painting (Painting, arc, fillColor, filled, translate, withMove)
import Math (pi)
import SambaDeUmaNotaSo.Constants (beats)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

type IPContext a
  = Reader { canvas :: DOMRect, halfW :: Number, halfH :: Number, mwh :: Number } a

singleWedge :: Int -> IPContext Painting
singleWedge n = do
  { canvas: { width, height }, halfW, halfH, mwh } <- ask
  pure $ (translate (width * -0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * st) (pi * ed) (1.000 * mwh * 0.45))))
  where
  st = toNumber n / 6.0

  ed = toNumber (n + 1) / 6.0

wedge13 :: DOMRect -> Painting
wedge13 { width, height } = (translate (width * -0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.5) (pi * 0.6666666666666666) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * -0.16666666666666666) (pi * 0.0) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.16666666666666666) (pi * 0.3333333333333333) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.0) (pi * 1.1666666666666667) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.6666666666666666) (pi * 0.8333333333333334) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.6666666666666667) (pi * 1.8333333333333333) (1.000 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height
