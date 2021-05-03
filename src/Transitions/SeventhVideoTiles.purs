module SambaDeUmaNotaSo.SeventhVideoTiles where

import Prelude
import Color (rgba)
import Data.Typelevel.Num (D32)
import Data.Vec ((+>))
import Data.Vec as V
import SambaDeUmaNotaSo.TileTypes (TileBuilder, TileBuilder2, tbToTb2)

seventhTilesForeground' :: V.Vec D32 TileBuilder
seventhTilesForeground' =
  { color: rgba 236 163 245 1.0, x0: 139.0, x1: 235.0, y0: 0.0, y1: 236.0 }
    +> { color: rgba 237 255 169 1.0, x0: 0.0, x1: 97.0, y0: 174.0, y1: 264.0 }
    +> { color: rgba 253 186 248 1.0, x0: 98.0, x1: 160.0, y0: 237.0, y1: 291.0 }
    +> { color: rgba 152 221 202 1.0, x0: 0.0, x1: 97.0, y0: 265.0, y1: 511.0 }
    +> { color: rgba 237 255 169 1.0, x0: 395.0, x1: 451.0, y0: 449.0, y1: 461.0 }
    +> { color: rgba 255 170 167 1.0, x0: 0.0, x1: 138.0, y0: 0.0, y1: 125.0 }
    +> { color: rgba 152 221 202 1.0, x0: 469.0, x1: 481.0, y0: 432.0, y1: 484.0 }
    +> { color: rgba 176 239 235 1.0, x0: 98.0, x1: 286.0, y0: 303.0, y1: 463.0 }
    +> { color: rgba 236 163 245 1.0, x0: 287.0, x1: 368.0, y0: 0.0, y1: 384.0 }
    +> { color: rgba 255 211 180 1.0, x0: 236.0, x1: 286.0, y0: 17.0, y1: 249.0 }
    +> { color: rgba 236 163 245 1.0, x0: 369.0, x1: 511.0, y0: 48.0, y1: 312.0 }
    +> { color: rgba 255 170 167 1.0, x0: 482.0, x1: 511.0, y0: 313.0, y1: 511.0 }
    +> { color: rgba 176 239 235 1.0, x0: 337.0, x1: 468.0, y0: 462.0, y1: 511.0 }
    +> { color: rgba 255 211 180 1.0, x0: 161.0, x1: 235.0, y0: 237.0, y1: 302.0 }
    +> { color: rgba 236 163 245 1.0, x0: 369.0, x1: 481.0, y0: 313.0, y1: 431.0 }
    +> { color: rgba 255 170 167 1.0, x0: 297.0, x1: 394.0, y0: 432.0, y1: 461.0 }
    +> { color: rgba 255 170 167 1.0, x0: 0.0, x1: 138.0, y0: 126.0, y1: 173.0 }
    +> { color: rgba 152 221 202 1.0, x0: 98.0, x1: 336.0, y0: 464.0, y1: 511.0 }
    +> { color: rgba 255 170 167 1.0, x0: 236.0, x1: 286.0, y0: 250.0, y1: 302.0 }
    +> { color: rgba 253 186 248 1.0, x0: 287.0, x1: 368.0, y0: 385.0, y1: 431.0 }
    +> { color: rgba 253 186 248 1.0, x0: 437.0, x1: 493.0, y0: 0.0, y1: 47.0 }
    +> { color: rgba 176 239 235 1.0, x0: 98.0, x1: 138.0, y0: 174.0, y1: 236.0 }
    +> { color: rgba 152 221 202 1.0, x0: 494.0, x1: 511.0, y0: 0.0, y1: 47.0 }
    +> { color: rgba 255 211 180 1.0, x0: 395.0, x1: 468.0, y0: 432.0, y1: 448.0 }
    +> { color: rgba 176 239 235 1.0, x0: 369.0, x1: 436.0, y0: 0.0, y1: 27.0 }
    +> { color: rgba 255 211 180 1.0, x0: 452.0, x1: 468.0, y0: 449.0, y1: 461.0 }
    +> { color: rgba 255 211 180 1.0, x0: 236.0, x1: 286.0, y0: 0.0, y1: 16.0 }
    +> { color: rgba 253 186 248 1.0, x0: 369.0, x1: 436.0, y0: 28.0, y1: 47.0 }
    +> { color: rgba 255 170 167 1.0, x0: 469.0, x1: 481.0, y0: 485.0, y1: 511.0 }
    +> { color: rgba 255 170 167 1.0, x0: 287.0, x1: 296.0, y0: 432.0, y1: 463.0 }
    +> { color: rgba 213 236 194 1.0, x0: 297.0, x1: 336.0, y0: 462.0, y1: 463.0 }
    +> { color: rgba 253 186 248 1.0, x0: 98.0, x1: 160.0, y0: 292.0, y1: 302.0 }
    +> V.empty

tiles7 :: V.Vec D32 TileBuilder2
tiles7 = map tbToTb2 seventhTilesForeground'
