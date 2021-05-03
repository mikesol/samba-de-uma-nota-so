module SambaDeUmaNotaSo.TileTypes where

import Prelude
import Color (Color)

type TileBuilder
  = { color :: Color
    , x0 :: Number
    , x1 :: Number
    , y0 :: Number
    , y1 :: Number
    }

type TileBuilder2
  = { color :: Color
    , x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
    }

squareSize = 512.0 :: Number

tbToTb2 :: TileBuilder -> TileBuilder2
tbToTb2 i = { color: i.color, x: i.x0 / squareSize, width: (i.x1 + 1.0 - i.x0) / squareSize, y: i.y0 / squareSize, height: (i.y1 + 1.0 - i.y0) / squareSize }
