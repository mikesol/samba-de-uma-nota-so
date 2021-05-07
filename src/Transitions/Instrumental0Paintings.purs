module SambaDeUmaNotaSo.Instrumental0Paintings where

import Prelude
import Color (rgb)
import Control.Monad.Reader (Reader, ask)
import Data.Int (toNumber)
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Graphics.Painting (Painting, arc, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate, withMove)
import Math (pi)
import SambaDeUmaNotaSo.Constants (beats)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

type IPContext a
  = Reader
      { canvas :: DOMRect
      , halfW :: Number
      , halfH :: Number
      , mwh :: Number
      }
      a

background :: IPContext Painting
background = do
  { canvas: { width, height } } <- ask
  pure $ filled (fillColor (rgb 24 83 76)) (rectangle 0.0 0.0 width height)

innerCircle :: IPContext Painting
innerCircle = do
  { halfW, halfH, mwh } <- ask
  pure $ filled (fillColor (rgb 200 200 200)) (circle halfW halfH (mwh * 0.06))

ring0 :: IPContext Painting
ring0 = do
  { halfW, halfH, mwh } <- ask
  pure $ outlined (outlineColor (rgb 100 100 100) <> lineWidth (mwh * 0.05)) (circle halfW halfH (mwh * 0.25))

ring1 :: IPContext Painting
ring1 = do
  { halfW, halfH, mwh } <- ask
  pure $ outlined (outlineColor (rgb 100 100 100) <> lineWidth (mwh * 0.05)) (circle halfW halfH (mwh * 0.34))

singleWedge :: Int -> IPContext Painting
singleWedge n = do
  { canvas: { width, height }, halfW, halfH, mwh } <- ask
  pure $ (translate (width * -0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * st) (pi * ed) (1.000 * mwh * 0.45))))
  where
  st = toNumber n / 6.0

  ed = toNumber (n + 1) / 6.0
