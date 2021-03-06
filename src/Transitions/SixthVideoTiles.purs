module SambaDeUmaNotaSo.SixthVideoTiles where

import Prelude
import Color (Color, rgb, rgba)
import Data.List ((:), List(..))
import Data.Typelevel.Num (D16, d16)
import Data.Vec ((+>))
import Data.Vec as V
import SambaDeUmaNotaSo.TileTypes (TileBuilder, TileBuilder2)

colorA0 = rgba 253 186 248 0.5 :: Color

colorA1 = rgba 255 170 167 0.52 :: Color

colorA2 = rgba 176 239 235 0.58 :: Color

colorA3 = rgba 213 236 194 0.45 :: Color

colorA4 = rgba 237 255 169 0.59 :: Color

colorA5 = rgba 255 211 180 0.61 :: Color

colorA6 = rgba 152 221 202 0.63 :: Color

colorA7 = rgba 236 163 245 0.42 :: Color

colorB0 = rgb 253 186 248 :: Color

colorB1 = rgb 255 170 167 :: Color

colorB2 = rgb 176 239 235 :: Color

colorB3 = rgb 213 236 194 :: Color

colorB4 = rgb 237 255 169 :: Color

colorB5 = rgb 255 211 180 :: Color

colorB6 = rgb 152 221 202 :: Color

colorB7 = rgb 236 163 245 :: Color

tiles :: V.Vec D16 (List TileBuilder)
tiles =
  ( { color: colorA0, x0: 215.0, x1: 431.0, y0: 90.0, y1: 354.0 }
      : { color: colorA1, x0: 23.0, x1: 113.0, y0: 301.0, y1: 403.0 }
      : { color: colorA2, x0: 195.0, x1: 511.0, y0: 0.0, y1: 89.0 }
      : { color: colorA0, x0: 447.0, x1: 511.0, y0: 162.0, y1: 511.0 }
      : { color: colorA3, x0: 114.0, x1: 446.0, y0: 355.0, y1: 468.0 }
      : { color: colorA4, x0: 96.0, x1: 168.0, y0: 63.0, y1: 225.0 }
      : { color: colorA6, x0: 104.0, x1: 194.0, y0: 0.0, y1: 62.0 }
      : { color: colorA6, x0: 0.0, x1: 446.0, y0: 469.0, y1: 511.0 }
      : { color: colorA1, x0: 448.0, x1: 511.0, y0: 90.0, y1: 161.0 }
      : { color: colorA5, x0: 169.0, x1: 214.0, y0: 90.0, y1: 354.0 }
      : { color: colorA2, x0: 0.0, x1: 95.0, y0: 56.0, y1: 200.0 }
      : { color: colorA0, x0: 432.0, x1: 446.0, y0: 90.0, y1: 354.0 }
      : { color: colorA4, x0: 0.0, x1: 75.0, y0: 201.0, y1: 300.0 }
      : { color: colorA5, x0: 114.0, x1: 168.0, y0: 226.0, y1: 354.0 }
      : { color: colorA6, x0: 0.0, x1: 91.0, y0: 404.0, y1: 468.0 }
      : { color: colorA4, x0: 76.0, x1: 95.0, y0: 201.0, y1: 300.0 }
      : { color: colorA5, x0: 45.0, x1: 103.0, y0: 0.0, y1: 55.0 }
      : { color: colorA0, x0: 0.0, x1: 22.0, y0: 301.0, y1: 403.0 }
      : { color: colorA6, x0: 169.0, x1: 194.0, y0: 63.0, y1: 89.0 }
      : { color: colorA2, x0: 96.0, x1: 113.0, y0: 226.0, y1: 300.0 }
      : { color: colorA3, x0: 92.0, x1: 113.0, y0: 404.0, y1: 445.0 }
      : { color: colorA5, x0: 0.0, x1: 44.0, y0: 0.0, y1: 55.0 }
      : { color: colorA3, x0: 92.0, x1: 113.0, y0: 446.0, y1: 468.0 }
      : { color: colorA6, x0: 447.0, x1: 447.0, y0: 90.0, y1: 161.0 }
      : { color: colorA6, x0: 96.0, x1: 103.0, y0: 56.0, y1: 62.0 }
      : Nil
  )
    +> ( { color: colorA0, x0: 89.0, x1: 287.0, y0: 224.0, y1: 350.0 }
          : { color: colorA4, x0: 151.0, x1: 291.0, y0: 81.0, y1: 201.0 }
          : { color: colorA7, x0: 340.0, x1: 464.0, y0: 437.0, y1: 501.0 }
          : { color: colorA1, x0: 292.0, x1: 425.0, y0: 58.0, y1: 232.0 }
          : { color: colorA7, x0: 0.0, x1: 511.0, y0: 0.0, y1: 57.0 }
          : { color: colorA0, x0: 0.0, x1: 88.0, y0: 58.0, y1: 358.0 }
          : { color: colorA0, x0: 426.0, x1: 511.0, y0: 58.0, y1: 436.0 }
          : { color: colorA6, x0: 238.0, x1: 425.0, y0: 370.0, y1: 436.0 }
          : { color: colorA1, x0: 0.0, x1: 511.0, y0: 502.0, y1: 511.0 }
          : { color: colorA5, x0: 279.0, x1: 339.0, y0: 437.0, y1: 501.0 }
          : { color: colorA1, x0: 465.0, x1: 511.0, y0: 437.0, y1: 501.0 }
          : { color: colorA1, x0: 89.0, x1: 150.0, y0: 58.0, y1: 223.0 }
          : { color: colorA0, x0: 0.0, x1: 237.0, y0: 359.0, y1: 501.0 }
          : { color: colorA7, x0: 288.0, x1: 394.0, y0: 233.0, y1: 306.0 }
          : { color: colorA0, x0: 89.0, x1: 425.0, y0: 351.0, y1: 358.0 }
          : { color: colorA3, x0: 151.0, x1: 291.0, y0: 202.0, y1: 223.0 }
          : { color: colorA6, x0: 395.0, x1: 425.0, y0: 233.0, y1: 350.0 }
          : { color: colorA0, x0: 151.0, x1: 291.0, y0: 58.0, y1: 80.0 }
          : { color: colorA5, x0: 238.0, x1: 425.0, y0: 359.0, y1: 369.0 }
          : { color: colorA4, x0: 288.0, x1: 394.0, y0: 307.0, y1: 350.0 }
          : { color: colorA7, x0: 238.0, x1: 278.0, y0: 437.0, y1: 501.0 }
          : { color: colorA6, x0: 288.0, x1: 291.0, y0: 224.0, y1: 232.0 }
          : Nil
      )
    +> ( { color: colorA1, x0: 357.0, x1: 397.0, y0: 12.0, y1: 112.0 }
          : { color: colorA0, x0: 0.0, x1: 167.0, y0: 0.0, y1: 126.0 }
          : { color: colorA4, x0: 136.0, x1: 312.0, y0: 234.0, y1: 290.0 }
          : { color: colorA6, x0: 51.0, x1: 177.0, y0: 141.0, y1: 165.0 }
          : { color: colorA4, x0: 313.0, x1: 511.0, y0: 241.0, y1: 331.0 }
          : { color: colorA1, x0: 359.0, x1: 511.0, y0: 439.0, y1: 511.0 }
          : { color: colorA1, x0: 210.0, x1: 511.0, y0: 154.0, y1: 233.0 }
          : { color: colorA1, x0: 78.0, x1: 511.0, y0: 332.0, y1: 438.0 }
          : { color: colorA2, x0: 237.0, x1: 291.0, y0: 73.0, y1: 153.0 }
          : { color: colorA6, x0: 0.0, x1: 159.0, y0: 450.0, y1: 511.0 }
          : { color: colorA2, x0: 398.0, x1: 511.0, y0: 0.0, y1: 153.0 }
          : { color: colorA6, x0: 160.0, x1: 358.0, y0: 439.0, y1: 511.0 }
          : { color: colorA1, x0: 306.0, x1: 358.0, y0: 113.0, y1: 153.0 }
          : { color: colorA0, x0: 0.0, x1: 77.0, y0: 254.0, y1: 449.0 }
          : { color: colorA1, x0: 210.0, x1: 356.0, y0: 0.0, y1: 72.0 }
          : { color: colorA6, x0: 78.0, x1: 135.0, y0: 166.0, y1: 331.0 }
          : { color: colorA5, x0: 136.0, x1: 312.0, y0: 291.0, y1: 331.0 }
          : { color: colorA5, x0: 136.0, x1: 209.0, y0: 166.0, y1: 233.0 }
          : { color: colorA5, x0: 292.0, x1: 356.0, y0: 73.0, y1: 112.0 }
          : { color: colorA6, x0: 359.0, x1: 397.0, y0: 113.0, y1: 153.0 }
          : { color: colorA1, x0: 0.0, x1: 77.0, y0: 166.0, y1: 253.0 }
          : { color: colorA4, x0: 168.0, x1: 236.0, y0: 73.0, y1: 140.0 }
          : { color: colorA4, x0: 0.0, x1: 167.0, y0: 127.0, y1: 140.0 }
          : { color: colorA0, x0: 168.0, x1: 209.0, y0: 0.0, y1: 72.0 }
          : { color: colorA2, x0: 0.0, x1: 50.0, y0: 141.0, y1: 165.0 }
          : { color: colorA5, x0: 357.0, x1: 397.0, y0: 0.0, y1: 11.0 }
          : { color: colorA4, x0: 313.0, x1: 511.0, y0: 234.0, y1: 240.0 }
          : { color: colorA4, x0: 292.0, x1: 305.0, y0: 113.0, y1: 153.0 }
          : { color: colorA7, x0: 178.0, x1: 236.0, y0: 141.0, y1: 153.0 }
          : { color: colorA3, x0: 178.0, x1: 209.0, y0: 154.0, y1: 165.0 }
          : { color: colorA5, x0: 78.0, x1: 159.0, y0: 439.0, y1: 449.0 }
          : Nil
      )
    +> ( { color: colorA1, x0: 20.0, x1: 86.0, y0: 331.0, y1: 367.0 }
          : { color: colorA5, x0: 0.0, x1: 117.0, y0: 215.0, y1: 305.0 }
          : { color: colorA3, x0: 130.0, x1: 286.0, y0: 75.0, y1: 399.0 }
          : { color: colorA5, x0: 26.0, x1: 102.0, y0: 11.0, y1: 127.0 }
          : { color: colorA1, x0: 287.0, x1: 330.0, y0: 270.0, y1: 478.0 }
          : { color: colorA3, x0: 0.0, x1: 129.0, y0: 368.0, y1: 511.0 }
          : { color: colorA0, x0: 347.0, x1: 391.0, y0: 294.0, y1: 318.0 }
          : { color: colorA2, x0: 0.0, x1: 111.0, y0: 128.0, y1: 214.0 }
          : { color: colorA1, x0: 359.0, x1: 511.0, y0: 319.0, y1: 511.0 }
          : { color: colorA4, x0: 366.0, x1: 424.0, y0: 195.0, y1: 235.0 }
          : { color: colorA0, x0: 0.0, x1: 25.0, y0: 0.0, y1: 127.0 }
          : { color: colorA5, x0: 436.0, x1: 511.0, y0: 128.0, y1: 228.0 }
          : { color: colorA2, x0: 130.0, x1: 227.0, y0: 400.0, y1: 511.0 }
          : { color: colorA6, x0: 392.0, x1: 511.0, y0: 236.0, y1: 318.0 }
          : { color: colorA6, x0: 118.0, x1: 129.0, y0: 0.0, y1: 367.0 }
          : { color: colorA1, x0: 130.0, x1: 230.0, y0: 0.0, y1: 74.0 }
          : { color: colorA1, x0: 331.0, x1: 358.0, y0: 319.0, y1: 511.0 }
          : { color: colorA6, x0: 287.0, x1: 511.0, y0: 0.0, y1: 127.0 }
          : { color: colorA3, x0: 287.0, x1: 435.0, y0: 128.0, y1: 194.0 }
          : { color: colorA7, x0: 287.0, x1: 365.0, y0: 195.0, y1: 269.0 }
          : { color: colorA4, x0: 228.0, x1: 286.0, y0: 400.0, y1: 499.0 }
          : { color: colorA5, x0: 26.0, x1: 117.0, y0: 0.0, y1: 10.0 }
          : { color: colorA5, x0: 87.0, x1: 117.0, y0: 306.0, y1: 367.0 }
          : { color: colorA2, x0: 425.0, x1: 435.0, y0: 195.0, y1: 235.0 }
          : { color: colorA5, x0: 0.0, x1: 19.0, y0: 306.0, y1: 367.0 }
          : { color: colorA2, x0: 287.0, x1: 330.0, y0: 479.0, y1: 511.0 }
          : { color: colorA5, x0: 103.0, x1: 117.0, y0: 11.0, y1: 127.0 }
          : { color: colorA5, x0: 231.0, x1: 286.0, y0: 0.0, y1: 74.0 }
          : { color: colorA6, x0: 331.0, x1: 391.0, y0: 270.0, y1: 293.0 }
          : { color: colorA5, x0: 436.0, x1: 511.0, y0: 229.0, y1: 235.0 }
          : { color: colorA4, x0: 20.0, x1: 86.0, y0: 306.0, y1: 330.0 }
          : { color: colorA7, x0: 366.0, x1: 391.0, y0: 236.0, y1: 269.0 }
          : { color: colorA7, x0: 331.0, x1: 346.0, y0: 294.0, y1: 318.0 }
          : { color: colorA6, x0: 228.0, x1: 286.0, y0: 500.0, y1: 511.0 }
          : { color: colorA1, x0: 112.0, x1: 117.0, y0: 128.0, y1: 214.0 }
          : Nil
      )
    +> ( { color: colorA2, x0: 222.0, x1: 262.0, y0: 0.0, y1: 244.0 }
          : { color: colorA5, x0: 362.0, x1: 464.0, y0: 38.0, y1: 134.0 }
          : { color: colorA2, x0: 316.0, x1: 361.0, y0: 62.0, y1: 278.0 }
          : { color: colorA5, x0: 63.0, x1: 187.0, y0: 317.0, y1: 333.0 }
          : { color: colorA6, x0: 331.0, x1: 447.0, y0: 326.0, y1: 382.0 }
          : { color: colorA3, x0: 32.0, x1: 128.0, y0: 377.0, y1: 457.0 }
          : { color: colorA3, x0: 41.0, x1: 221.0, y0: 81.0, y1: 316.0 }
          : { color: colorA2, x0: 362.0, x1: 436.0, y0: 135.0, y1: 281.0 }
          : { color: colorA0, x0: 448.0, x1: 511.0, y0: 135.0, y1: 511.0 }
          : { color: colorA3, x0: 465.0, x1: 511.0, y0: 17.0, y1: 134.0 }
          : { color: colorA4, x0: 0.0, x1: 97.0, y0: 0.0, y1: 59.0 }
          : { color: colorA6, x0: 0.0, x1: 320.0, y0: 458.0, y1: 511.0 }
          : { color: colorA3, x0: 321.0, x1: 447.0, y0: 383.0, y1: 511.0 }
          : { color: colorA1, x0: 263.0, x1: 315.0, y0: 0.0, y1: 457.0 }
          : { color: colorA2, x0: 98.0, x1: 221.0, y0: 0.0, y1: 80.0 }
          : { color: colorA6, x0: 188.0, x1: 262.0, y0: 317.0, y1: 457.0 }
          : { color: colorA7, x0: 0.0, x1: 187.0, y0: 334.0, y1: 376.0 }
          : { color: colorA7, x0: 0.0, x1: 40.0, y0: 60.0, y1: 333.0 }
          : { color: colorA7, x0: 129.0, x1: 187.0, y0: 377.0, y1: 457.0 }
          : { color: colorA1, x0: 316.0, x1: 447.0, y0: 282.0, y1: 325.0 }
          : { color: colorA2, x0: 316.0, x1: 464.0, y0: 0.0, y1: 37.0 }
          : { color: colorA3, x0: 222.0, x1: 262.0, y0: 245.0, y1: 316.0 }
          : { color: colorA4, x0: 316.0, x1: 361.0, y0: 279.0, y1: 281.0 }
          : { color: colorA5, x0: 316.0, x1: 330.0, y0: 326.0, y1: 382.0 }
          : { color: colorA4, x0: 41.0, x1: 97.0, y0: 60.0, y1: 80.0 }
          : { color: colorA0, x0: 0.0, x1: 31.0, y0: 377.0, y1: 457.0 }
          : { color: colorA4, x0: 316.0, x1: 320.0, y0: 383.0, y1: 457.0 }
          : { color: colorA6, x0: 437.0, x1: 447.0, y0: 135.0, y1: 281.0 }
          : { color: colorA5, x0: 316.0, x1: 361.0, y0: 38.0, y1: 61.0 }
          : { color: colorA7, x0: 41.0, x1: 62.0, y0: 317.0, y1: 333.0 }
          : { color: colorA2, x0: 465.0, x1: 511.0, y0: 0.0, y1: 16.0 }
          : Nil
      )
    +> ( { color: colorA0, x0: 397.0, x1: 463.0, y0: 330.0, y1: 492.0 }
          : { color: colorA7, x0: 197.0, x1: 249.0, y0: 0.0, y1: 88.0 }
          : { color: colorA5, x0: 146.0, x1: 434.0, y0: 200.0, y1: 308.0 }
          : { color: colorA1, x0: 360.0, x1: 430.0, y0: 0.0, y1: 199.0 }
          : { color: colorA4, x0: 464.0, x1: 511.0, y0: 0.0, y1: 511.0 }
          : { color: colorA4, x0: 181.0, x1: 241.0, y0: 351.0, y1: 483.0 }
          : { color: colorA0, x0: 0.0, x1: 196.0, y0: 0.0, y1: 199.0 }
          : { color: colorA6, x0: 0.0, x1: 31.0, y0: 225.0, y1: 287.0 }
          : { color: colorA1, x0: 242.0, x1: 317.0, y0: 309.0, y1: 428.0 }
          : { color: colorA5, x0: 0.0, x1: 180.0, y0: 355.0, y1: 475.0 }
          : { color: colorA1, x0: 0.0, x1: 241.0, y0: 309.0, y1: 350.0 }
          : { color: colorA7, x0: 435.0, x1: 463.0, y0: 0.0, y1: 329.0 }
          : { color: colorA7, x0: 378.0, x1: 463.0, y0: 493.0, y1: 511.0 }
          : { color: colorA3, x0: 318.0, x1: 434.0, y0: 309.0, y1: 329.0 }
          : { color: colorA4, x0: 318.0, x1: 396.0, y0: 330.0, y1: 492.0 }
          : { color: colorA7, x0: 250.0, x1: 359.0, y0: 0.0, y1: 199.0 }
          : { color: colorA4, x0: 260.0, x1: 317.0, y0: 429.0, y1: 484.0 }
          : { color: colorA2, x0: 197.0, x1: 249.0, y0: 89.0, y1: 199.0 }
          : { color: colorA2, x0: 62.0, x1: 124.0, y0: 200.0, y1: 259.0 }
          : { color: colorA6, x0: 0.0, x1: 259.0, y0: 484.0, y1: 511.0 }
          : { color: colorA3, x0: 242.0, x1: 259.0, y0: 429.0, y1: 483.0 }
          : { color: colorA2, x0: 0.0, x1: 180.0, y0: 351.0, y1: 354.0 }
          : { color: colorA3, x0: 260.0, x1: 377.0, y0: 493.0, y1: 511.0 }
          : { color: colorA3, x0: 32.0, x1: 61.0, y0: 200.0, y1: 308.0 }
          : { color: colorA1, x0: 431.0, x1: 434.0, y0: 0.0, y1: 199.0 }
          : { color: colorA0, x0: 125.0, x1: 145.0, y0: 200.0, y1: 308.0 }
          : { color: colorA2, x0: 62.0, x1: 124.0, y0: 260.0, y1: 308.0 }
          : { color: colorA4, x0: 0.0, x1: 180.0, y0: 476.0, y1: 483.0 }
          : { color: colorA5, x0: 260.0, x1: 317.0, y0: 485.0, y1: 492.0 }
          : { color: colorA2, x0: 0.0, x1: 31.0, y0: 288.0, y1: 308.0 }
          : { color: colorA1, x0: 0.0, x1: 31.0, y0: 200.0, y1: 224.0 }
          : Nil
      )
    +> ( { color: colorA2, x0: 29.0, x1: 185.0, y0: 117.0, y1: 255.0 }
          : { color: colorA3, x0: 186.0, x1: 319.0, y0: 142.0, y1: 256.0 }
          : { color: colorA1, x0: 185.0, x1: 245.0, y0: 257.0, y1: 376.0 }
          : { color: colorA5, x0: 320.0, x1: 511.0, y0: 0.0, y1: 271.0 }
          : { color: colorA6, x0: 288.0, x1: 448.0, y0: 272.0, y1: 440.0 }
          : { color: colorA5, x0: 263.0, x1: 319.0, y0: 39.0, y1: 141.0 }
          : { color: colorA2, x0: 14.0, x1: 50.0, y0: 430.0, y1: 490.0 }
          : { color: colorA5, x0: 0.0, x1: 198.0, y0: 8.0, y1: 96.0 }
          : { color: colorA1, x0: 51.0, x1: 184.0, y0: 256.0, y1: 511.0 }
          : { color: colorA3, x0: 202.0, x1: 310.0, y0: 441.0, y1: 511.0 }
          : { color: colorA3, x0: 0.0, x1: 28.0, y0: 97.0, y1: 429.0 }
          : { color: colorA4, x0: 201.0, x1: 287.0, y0: 377.0, y1: 440.0 }
          : { color: colorA6, x0: 185.0, x1: 201.0, y0: 441.0, y1: 511.0 }
          : { color: colorA1, x0: 0.0, x1: 319.0, y0: 0.0, y1: 7.0 }
          : { color: colorA7, x0: 199.0, x1: 262.0, y0: 8.0, y1: 141.0 }
          : { color: colorA1, x0: 263.0, x1: 319.0, y0: 8.0, y1: 38.0 }
          : { color: colorA2, x0: 246.0, x1: 287.0, y0: 257.0, y1: 376.0 }
          : { color: colorA5, x0: 449.0, x1: 511.0, y0: 290.0, y1: 378.0 }
          : { color: colorA2, x0: 29.0, x1: 50.0, y0: 256.0, y1: 429.0 }
          : { color: colorA6, x0: 449.0, x1: 511.0, y0: 379.0, y1: 511.0 }
          : { color: colorA4, x0: 29.0, x1: 198.0, y0: 97.0, y1: 116.0 }
          : { color: colorA4, x0: 311.0, x1: 448.0, y0: 441.0, y1: 478.0 }
          : { color: colorA6, x0: 186.0, x1: 198.0, y0: 117.0, y1: 141.0 }
          : { color: colorA6, x0: 311.0, x1: 448.0, y0: 479.0, y1: 511.0 }
          : { color: colorA5, x0: 449.0, x1: 511.0, y0: 272.0, y1: 289.0 }
          : { color: colorA2, x0: 288.0, x1: 319.0, y0: 257.0, y1: 271.0 }
          : { color: colorA4, x0: 185.0, x1: 200.0, y0: 377.0, y1: 440.0 }
          : { color: colorA6, x0: 0.0, x1: 50.0, y0: 491.0, y1: 511.0 }
          : { color: colorA7, x0: 0.0, x1: 13.0, y0: 430.0, y1: 490.0 }
          : { color: colorA2, x0: 185.0, x1: 185.0, y0: 256.0, y1: 256.0 }
          : Nil
      )
    +> ( { color: colorA7, x0: 271.0, x1: 319.0, y0: 317.0, y1: 511.0 }
          : { color: colorA0, x0: 180.0, x1: 511.0, y0: 208.0, y1: 316.0 }
          : { color: colorA2, x0: 116.0, x1: 242.0, y0: 109.0, y1: 207.0 }
          : { color: colorA2, x0: 109.0, x1: 245.0, y0: 367.0, y1: 487.0 }
          : { color: colorA2, x0: 353.0, x1: 461.0, y0: 339.0, y1: 511.0 }
          : { color: colorA7, x0: 41.0, x1: 179.0, y0: 208.0, y1: 321.0 }
          : { color: colorA1, x0: 320.0, x1: 352.0, y0: 317.0, y1: 511.0 }
          : { color: colorA1, x0: 203.0, x1: 331.0, y0: 42.0, y1: 90.0 }
          : { color: colorA0, x0: 243.0, x1: 511.0, y0: 91.0, y1: 207.0 }
          : { color: colorA6, x0: 0.0, x1: 108.0, y0: 322.0, y1: 511.0 }
          : { color: colorA6, x0: 0.0, x1: 115.0, y0: 0.0, y1: 207.0 }
          : { color: colorA7, x0: 116.0, x1: 242.0, y0: 91.0, y1: 108.0 }
          : { color: colorA2, x0: 116.0, x1: 202.0, y0: 0.0, y1: 90.0 }
          : { color: colorA0, x0: 332.0, x1: 511.0, y0: 0.0, y1: 90.0 }
          : { color: colorA1, x0: 203.0, x1: 331.0, y0: 0.0, y1: 41.0 }
          : { color: colorA1, x0: 0.0, x1: 40.0, y0: 208.0, y1: 321.0 }
          : { color: colorA4, x0: 180.0, x1: 270.0, y0: 317.0, y1: 366.0 }
          : { color: colorA3, x0: 462.0, x1: 511.0, y0: 317.0, y1: 511.0 }
          : { color: colorA3, x0: 246.0, x1: 270.0, y0: 367.0, y1: 511.0 }
          : { color: colorA6, x0: 109.0, x1: 179.0, y0: 322.0, y1: 366.0 }
          : { color: colorA1, x0: 109.0, x1: 245.0, y0: 488.0, y1: 511.0 }
          : { color: colorA7, x0: 353.0, x1: 461.0, y0: 317.0, y1: 338.0 }
          : Nil
      )
    +> ( { color: colorA1, x0: 328.0, x1: 511.0, y0: 200.0, y1: 296.0 }
          : { color: colorA5, x0: 404.0, x1: 511.0, y0: 400.0, y1: 484.0 }
          : { color: colorA1, x0: 159.0, x1: 327.0, y0: 121.0, y1: 373.0 }
          : { color: colorA0, x0: 16.0, x1: 403.0, y0: 401.0, y1: 511.0 }
          : { color: colorA4, x0: 328.0, x1: 511.0, y0: 55.0, y1: 199.0 }
          : { color: colorA6, x0: 0.0, x1: 158.0, y0: 206.0, y1: 400.0 }
          : { color: colorA2, x0: 159.0, x1: 403.0, y0: 374.0, y1: 400.0 }
          : { color: colorA3, x0: 404.0, x1: 500.0, y0: 385.0, y1: 399.0 }
          : { color: colorA3, x0: 50.0, x1: 186.0, y0: 0.0, y1: 95.0 }
          : { color: colorA4, x0: 187.0, x1: 511.0, y0: 0.0, y1: 54.0 }
          : { color: colorA7, x0: 0.0, x1: 158.0, y0: 96.0, y1: 205.0 }
          : { color: colorA2, x0: 208.0, x1: 327.0, y0: 55.0, y1: 120.0 }
          : { color: colorA0, x0: 404.0, x1: 511.0, y0: 297.0, y1: 384.0 }
          : { color: colorA3, x0: 404.0, x1: 511.0, y0: 485.0, y1: 511.0 }
          : { color: colorA6, x0: 0.0, x1: 49.0, y0: 0.0, y1: 95.0 }
          : { color: colorA6, x0: 159.0, x1: 207.0, y0: 96.0, y1: 120.0 }
          : { color: colorA3, x0: 0.0, x1: 15.0, y0: 401.0, y1: 511.0 }
          : { color: colorA6, x0: 328.0, x1: 403.0, y0: 297.0, y1: 373.0 }
          : { color: colorA5, x0: 187.0, x1: 207.0, y0: 55.0, y1: 95.0 }
          : { color: colorA4, x0: 501.0, x1: 511.0, y0: 385.0, y1: 399.0 }
          : Nil
      )
    +> ( { color: colorA3, x0: 121.0, x1: 201.0, y0: 0.0, y1: 133.0 }
          : { color: colorA0, x0: 0.0, x1: 120.0, y0: 0.0, y1: 511.0 }
          : { color: colorA3, x0: 121.0, x1: 238.0, y0: 171.0, y1: 207.0 }
          : { color: colorA2, x0: 357.0, x1: 409.0, y0: 301.0, y1: 361.0 }
          : { color: colorA2, x0: 254.0, x1: 474.0, y0: 239.0, y1: 300.0 }
          : { color: colorA5, x0: 219.0, x1: 303.0, y0: 310.0, y1: 511.0 }
          : { color: colorA2, x0: 371.0, x1: 511.0, y0: 0.0, y1: 238.0 }
          : { color: colorA6, x0: 304.0, x1: 356.0, y0: 301.0, y1: 511.0 }
          : { color: colorA6, x0: 169.0, x1: 205.0, y0: 361.0, y1: 425.0 }
          : { color: colorA4, x0: 475.0, x1: 511.0, y0: 239.0, y1: 511.0 }
          : { color: colorA3, x0: 121.0, x1: 218.0, y0: 208.0, y1: 360.0 }
          : { color: colorA1, x0: 255.0, x1: 370.0, y0: 66.0, y1: 126.0 }
          : { color: colorA6, x0: 121.0, x1: 284.0, y0: 134.0, y1: 170.0 }
          : { color: colorA2, x0: 285.0, x1: 370.0, y0: 127.0, y1: 238.0 }
          : { color: colorA5, x0: 357.0, x1: 463.0, y0: 412.0, y1: 511.0 }
          : { color: colorA7, x0: 202.0, x1: 370.0, y0: 0.0, y1: 65.0 }
          : { color: colorA0, x0: 219.0, x1: 284.0, y0: 208.0, y1: 238.0 }
          : { color: colorA4, x0: 202.0, x1: 240.0, y0: 66.0, y1: 87.0 }
          : { color: colorA2, x0: 464.0, x1: 474.0, y0: 301.0, y1: 511.0 }
          : { color: colorA2, x0: 239.0, x1: 284.0, y0: 171.0, y1: 207.0 }
          : { color: colorA6, x0: 219.0, x1: 253.0, y0: 239.0, y1: 309.0 }
          : { color: colorA2, x0: 121.0, x1: 168.0, y0: 361.0, y1: 511.0 }
          : { color: colorA0, x0: 202.0, x1: 254.0, y0: 88.0, y1: 133.0 }
          : { color: colorA1, x0: 190.0, x1: 218.0, y0: 477.0, y1: 511.0 }
          : { color: colorA0, x0: 357.0, x1: 463.0, y0: 362.0, y1: 411.0 }
          : { color: colorA1, x0: 410.0, x1: 463.0, y0: 301.0, y1: 361.0 }
          : { color: colorA5, x0: 206.0, x1: 218.0, y0: 361.0, y1: 476.0 }
          : { color: colorA5, x0: 169.0, x1: 205.0, y0: 426.0, y1: 476.0 }
          : { color: colorA3, x0: 254.0, x1: 303.0, y0: 301.0, y1: 309.0 }
          : { color: colorA2, x0: 169.0, x1: 189.0, y0: 477.0, y1: 511.0 }
          : { color: colorA1, x0: 241.0, x1: 254.0, y0: 66.0, y1: 87.0 }
          : { color: colorA0, x0: 255.0, x1: 284.0, y0: 127.0, y1: 133.0 }
          : Nil
      )
    +> ( { color: colorA6, x0: 297.0, x1: 401.0, y0: 282.0, y1: 338.0 }
          : { color: colorA5, x0: 15.0, x1: 105.0, y0: 193.0, y1: 253.0 }
          : { color: colorA5, x0: 197.0, x1: 381.0, y0: 339.0, y1: 486.0 }
          : { color: colorA5, x0: 382.0, x1: 511.0, y0: 438.0, y1: 494.0 }
          : { color: colorA4, x0: 0.0, x1: 98.0, y0: 97.0, y1: 192.0 }
          : { color: colorA0, x0: 0.0, x1: 279.0, y0: 280.0, y1: 328.0 }
          : { color: colorA6, x0: 247.0, x1: 511.0, y0: 111.0, y1: 167.0 }
          : { color: colorA2, x0: 123.0, x1: 315.0, y0: 220.0, y1: 268.0 }
          : { color: colorA0, x0: 402.0, x1: 439.0, y0: 236.0, y1: 392.0 }
          : { color: colorA3, x0: 440.0, x1: 511.0, y0: 375.0, y1: 437.0 }
          : { color: colorA4, x0: 77.0, x1: 196.0, y0: 329.0, y1: 498.0 }
          : { color: colorA0, x0: 167.0, x1: 215.0, y0: 102.0, y1: 150.0 }
          : { color: colorA3, x0: 106.0, x1: 218.0, y0: 151.0, y1: 219.0 }
          : { color: colorA1, x0: 0.0, x1: 511.0, y0: 499.0, y1: 511.0 }
          : { color: colorA4, x0: 0.0, x1: 401.0, y0: 269.0, y1: 279.0 }
          : { color: colorA1, x0: 463.0, x1: 511.0, y0: 201.0, y1: 374.0 }
          : { color: colorA5, x0: 316.0, x1: 374.0, y0: 186.0, y1: 268.0 }
          : { color: colorA5, x0: 99.0, x1: 358.0, y0: 0.0, y1: 101.0 }
          : { color: colorA2, x0: 0.0, x1: 76.0, y0: 329.0, y1: 498.0 }
          : { color: colorA5, x0: 0.0, x1: 98.0, y0: 0.0, y1: 96.0 }
          : { color: colorA7, x0: 0.0, x1: 122.0, y0: 254.0, y1: 268.0 }
          : { color: colorA6, x0: 375.0, x1: 511.0, y0: 168.0, y1: 200.0 }
          : { color: colorA7, x0: 375.0, x1: 462.0, y0: 201.0, y1: 235.0 }
          : { color: colorA2, x0: 359.0, x1: 511.0, y0: 0.0, y1: 110.0 }
          : { color: colorA4, x0: 382.0, x1: 401.0, y0: 339.0, y1: 437.0 }
          : { color: colorA2, x0: 219.0, x1: 315.0, y0: 168.0, y1: 219.0 }
          : { color: colorA2, x0: 440.0, x1: 462.0, y0: 236.0, y1: 374.0 }
          : { color: colorA7, x0: 402.0, x1: 439.0, y0: 393.0, y1: 437.0 }
          : { color: colorA4, x0: 375.0, x1: 401.0, y0: 236.0, y1: 268.0 }
          : { color: colorA1, x0: 316.0, x1: 374.0, y0: 168.0, y1: 185.0 }
          : { color: colorA2, x0: 197.0, x1: 381.0, y0: 487.0, y1: 498.0 }
          : { color: colorA6, x0: 0.0, x1: 14.0, y0: 193.0, y1: 253.0 }
          : { color: colorA3, x0: 99.0, x1: 166.0, y0: 102.0, y1: 150.0 }
          : { color: colorA3, x0: 280.0, x1: 296.0, y0: 280.0, y1: 338.0 }
          : { color: colorA5, x0: 382.0, x1: 511.0, y0: 495.0, y1: 498.0 }
          : { color: colorA4, x0: 216.0, x1: 358.0, y0: 102.0, y1: 110.0 }
          : { color: colorA4, x0: 297.0, x1: 401.0, y0: 280.0, y1: 281.0 }
          : { color: colorA6, x0: 197.0, x1: 279.0, y0: 329.0, y1: 338.0 }
          : { color: colorA2, x0: 216.0, x1: 246.0, y0: 111.0, y1: 150.0 }
          : { color: colorA4, x0: 99.0, x1: 105.0, y0: 151.0, y1: 192.0 }
          : { color: colorA6, x0: 106.0, x1: 122.0, y0: 220.0, y1: 253.0 }
          : { color: colorA0, x0: 219.0, x1: 246.0, y0: 151.0, y1: 167.0 }
          : Nil
      )
    +> ( { color: colorA2, x0: 0.0, x1: 182.0, y0: 25.0, y1: 145.0 }
          : { color: colorA6, x0: 0.0, x1: 153.0, y0: 389.0, y1: 511.0 }
          : { color: colorA3, x0: 0.0, x1: 138.0, y0: 239.0, y1: 309.0 }
          : { color: colorA3, x0: 183.0, x1: 511.0, y0: 0.0, y1: 98.0 }
          : { color: colorA6, x0: 0.0, x1: 511.0, y0: 146.0, y1: 226.0 }
          : { color: colorA6, x0: 139.0, x1: 273.0, y0: 227.0, y1: 388.0 }
          : { color: colorA5, x0: 215.0, x1: 359.0, y0: 389.0, y1: 511.0 }
          : { color: colorA0, x0: 274.0, x1: 511.0, y0: 227.0, y1: 388.0 }
          : { color: colorA6, x0: 0.0, x1: 138.0, y0: 310.0, y1: 388.0 }
          : { color: colorA4, x0: 154.0, x1: 214.0, y0: 389.0, y1: 511.0 }
          : { color: colorA0, x0: 10.0, x1: 182.0, y0: 0.0, y1: 24.0 }
          : { color: colorA5, x0: 0.0, x1: 138.0, y0: 227.0, y1: 238.0 }
          : { color: colorA5, x0: 183.0, x1: 511.0, y0: 99.0, y1: 145.0 }
          : { color: colorA5, x0: 443.0, x1: 511.0, y0: 403.0, y1: 511.0 }
          : { color: colorA3, x0: 381.0, x1: 442.0, y0: 389.0, y1: 511.0 }
          : { color: colorA7, x0: 360.0, x1: 380.0, y0: 389.0, y1: 511.0 }
          : { color: colorA4, x0: 443.0, x1: 511.0, y0: 389.0, y1: 402.0 }
          : { color: colorA2, x0: 0.0, x1: 9.0, y0: 0.0, y1: 24.0 }
          : Nil
      )
    +> ( { color: colorA0, x0: 138.0, x1: 290.0, y0: 260.0, y1: 484.0 }
          : { color: colorA1, x0: 318.0, x1: 492.0, y0: 52.0, y1: 214.0 }
          : { color: colorA6, x0: 303.0, x1: 343.0, y0: 284.0, y1: 484.0 }
          : { color: colorA0, x0: 0.0, x1: 317.0, y0: 0.0, y1: 203.0 }
          : { color: colorA0, x0: 368.0, x1: 452.0, y0: 464.0, y1: 511.0 }
          : { color: colorA4, x0: 0.0, x1: 48.0, y0: 268.0, y1: 358.0 }
          : { color: colorA2, x0: 49.0, x1: 137.0, y0: 204.0, y1: 511.0 }
          : { color: colorA0, x0: 344.0, x1: 437.0, y0: 215.0, y1: 359.0 }
          : { color: colorA2, x0: 438.0, x1: 480.0, y0: 269.0, y1: 463.0 }
          : { color: colorA2, x0: 138.0, x1: 317.0, y0: 204.0, y1: 259.0 }
          : { color: colorA4, x0: 469.0, x1: 511.0, y0: 480.0, y1: 511.0 }
          : { color: colorA7, x0: 493.0, x1: 511.0, y0: 0.0, y1: 479.0 }
          : { color: colorA1, x0: 318.0, x1: 492.0, y0: 0.0, y1: 51.0 }
          : { color: colorA6, x0: 0.0, x1: 48.0, y0: 359.0, y1: 511.0 }
          : { color: colorA7, x0: 138.0, x1: 367.0, y0: 485.0, y1: 511.0 }
          : { color: colorA2, x0: 291.0, x1: 302.0, y0: 260.0, y1: 484.0 }
          : { color: colorA5, x0: 453.0, x1: 468.0, y0: 464.0, y1: 511.0 }
          : { color: colorA3, x0: 0.0, x1: 48.0, y0: 204.0, y1: 267.0 }
          : { color: colorA7, x0: 344.0, x1: 437.0, y0: 360.0, y1: 463.0 }
          : { color: colorA1, x0: 481.0, x1: 492.0, y0: 215.0, y1: 479.0 }
          : { color: colorA7, x0: 303.0, x1: 343.0, y0: 260.0, y1: 283.0 }
          : { color: colorA1, x0: 318.0, x1: 343.0, y0: 215.0, y1: 259.0 }
          : { color: colorA7, x0: 438.0, x1: 480.0, y0: 215.0, y1: 268.0 }
          : { color: colorA2, x0: 344.0, x1: 367.0, y0: 464.0, y1: 484.0 }
          : { color: colorA2, x0: 469.0, x1: 480.0, y0: 464.0, y1: 479.0 }
          : Nil
      )
    +> ( { color: colorA0, x0: 377.0, x1: 429.0, y0: 123.0, y1: 171.0 }
          : { color: colorA5, x0: 218.0, x1: 318.0, y0: 107.0, y1: 163.0 }
          : { color: colorA6, x0: 23.0, x1: 309.0, y0: 281.0, y1: 511.0 }
          : { color: colorA4, x0: 310.0, x1: 511.0, y0: 400.0, y1: 511.0 }
          : { color: colorA6, x0: 169.0, x1: 511.0, y0: 172.0, y1: 280.0 }
          : { color: colorA1, x0: 417.0, x1: 485.0, y0: 308.0, y1: 399.0 }
          : { color: colorA3, x0: 0.0, x1: 142.0, y0: 24.0, y1: 192.0 }
          : { color: colorA6, x0: 310.0, x1: 416.0, y0: 281.0, y1: 356.0 }
          : { color: colorA1, x0: 341.0, x1: 455.0, y0: 77.0, y1: 122.0 }
          : { color: colorA5, x0: 143.0, x1: 217.0, y0: 0.0, y1: 171.0 }
          : { color: colorA0, x0: 245.0, x1: 511.0, y0: 0.0, y1: 76.0 }
          : { color: colorA0, x0: 218.0, x1: 244.0, y0: 0.0, y1: 106.0 }
          : { color: colorA4, x0: 22.0, x1: 168.0, y0: 193.0, y1: 280.0 }
          : { color: colorA2, x0: 0.0, x1: 142.0, y0: 0.0, y1: 23.0 }
          : { color: colorA6, x0: 0.0, x1: 21.0, y0: 193.0, y1: 511.0 }
          : { color: colorA0, x0: 430.0, x1: 511.0, y0: 123.0, y1: 171.0 }
          : { color: colorA5, x0: 353.0, x1: 416.0, y0: 357.0, y1: 399.0 }
          : { color: colorA6, x0: 245.0, x1: 340.0, y0: 77.0, y1: 106.0 }
          : { color: colorA2, x0: 456.0, x1: 511.0, y0: 77.0, y1: 122.0 }
          : { color: colorA0, x0: 310.0, x1: 352.0, y0: 357.0, y1: 399.0 }
          : { color: colorA5, x0: 143.0, x1: 168.0, y0: 172.0, y1: 192.0 }
          : { color: colorA3, x0: 319.0, x1: 376.0, y0: 123.0, y1: 171.0 }
          : { color: colorA3, x0: 486.0, x1: 511.0, y0: 281.0, y1: 399.0 }
          : { color: colorA0, x0: 417.0, x1: 485.0, y0: 281.0, y1: 307.0 }
          : { color: colorA1, x0: 218.0, x1: 318.0, y0: 164.0, y1: 171.0 }
          : { color: colorA2, x0: 22.0, x1: 22.0, y0: 281.0, y1: 511.0 }
          : { color: colorA1, x0: 319.0, x1: 340.0, y0: 107.0, y1: 122.0 }
          : Nil
      )
    +> ( { color: colorA1, x0: 0.0, x1: 174.0, y0: 299.0, y1: 511.0 }
          : { color: colorA0, x0: 291.0, x1: 403.0, y0: 261.0, y1: 501.0 }
          : { color: colorA2, x0: 175.0, x1: 290.0, y0: 316.0, y1: 400.0 }
          : { color: colorA4, x0: 286.0, x1: 358.0, y0: 166.0, y1: 260.0 }
          : { color: colorA5, x0: 132.0, x1: 500.0, y0: 25.0, y1: 165.0 }
          : { color: colorA2, x0: 175.0, x1: 290.0, y0: 401.0, y1: 511.0 }
          : { color: colorA1, x0: 75.0, x1: 131.0, y0: 0.0, y1: 260.0 }
          : { color: colorA1, x0: 404.0, x1: 511.0, y0: 330.0, y1: 408.0 }
          : { color: colorA5, x0: 404.0, x1: 511.0, y0: 409.0, y1: 511.0 }
          : { color: colorA5, x0: 0.0, x1: 74.0, y0: 0.0, y1: 298.0 }
          : { color: colorA0, x0: 192.0, x1: 285.0, y0: 206.0, y1: 315.0 }
          : { color: colorA7, x0: 326.0, x1: 434.0, y0: 0.0, y1: 24.0 }
          : { color: colorA3, x0: 359.0, x1: 511.0, y0: 166.0, y1: 260.0 }
          : { color: colorA1, x0: 132.0, x1: 191.0, y0: 166.0, y1: 298.0 }
          : { color: colorA0, x0: 203.0, x1: 325.0, y0: 0.0, y1: 24.0 }
          : { color: colorA7, x0: 404.0, x1: 511.0, y0: 261.0, y1: 329.0 }
          : { color: colorA2, x0: 435.0, x1: 511.0, y0: 0.0, y1: 24.0 }
          : { color: colorA5, x0: 75.0, x1: 131.0, y0: 261.0, y1: 298.0 }
          : { color: colorA6, x0: 175.0, x1: 191.0, y0: 299.0, y1: 315.0 }
          : { color: colorA4, x0: 501.0, x1: 511.0, y0: 25.0, y1: 165.0 }
          : { color: colorA1, x0: 192.0, x1: 285.0, y0: 166.0, y1: 205.0 }
          : { color: colorA3, x0: 291.0, x1: 403.0, y0: 502.0, y1: 511.0 }
          : { color: colorA1, x0: 132.0, x1: 202.0, y0: 0.0, y1: 24.0 }
          : { color: colorA6, x0: 286.0, x1: 290.0, y0: 261.0, y1: 315.0 }
          : Nil
      )
    +> ( { color: colorA2, x0: 190.0, x1: 346.0, y0: 119.0, y1: 299.0 }
          : { color: colorA3, x0: 405.0, x1: 465.0, y0: 345.0, y1: 511.0 }
          : { color: colorA1, x0: 347.0, x1: 511.0, y0: 256.0, y1: 326.0 }
          : { color: colorA6, x0: 196.0, x1: 404.0, y0: 327.0, y1: 421.0 }
          : { color: colorA3, x0: 84.0, x1: 189.0, y0: 0.0, y1: 209.0 }
          : { color: colorA5, x0: 347.0, x1: 511.0, y0: 131.0, y1: 255.0 }
          : { color: colorA6, x0: 204.0, x1: 456.0, y0: 0.0, y1: 118.0 }
          : { color: colorA3, x0: 41.0, x1: 161.0, y0: 390.0, y1: 511.0 }
          : { color: colorA6, x0: 0.0, x1: 83.0, y0: 128.0, y1: 224.0 }
          : { color: colorA0, x0: 466.0, x1: 511.0, y0: 327.0, y1: 511.0 }
          : { color: colorA3, x0: 239.0, x1: 323.0, y0: 434.0, y1: 511.0 }
          : { color: colorA6, x0: 457.0, x1: 511.0, y0: 0.0, y1: 130.0 }
          : { color: colorA7, x0: 24.0, x1: 40.0, y0: 31.0, y1: 127.0 }
          : { color: colorA0, x0: 41.0, x1: 83.0, y0: 0.0, y1: 127.0 }
          : { color: colorA6, x0: 162.0, x1: 238.0, y0: 422.0, y1: 511.0 }
          : { color: colorA3, x0: 0.0, x1: 346.0, y0: 300.0, y1: 326.0 }
          : { color: colorA2, x0: 0.0, x1: 40.0, y0: 344.0, y1: 511.0 }
          : { color: colorA4, x0: 41.0, x1: 195.0, y0: 327.0, y1: 389.0 }
          : { color: colorA0, x0: 324.0, x1: 404.0, y0: 422.0, y1: 511.0 }
          : { color: colorA6, x0: 405.0, x1: 465.0, y0: 327.0, y1: 344.0 }
          : { color: colorA7, x0: 0.0, x1: 189.0, y0: 225.0, y1: 299.0 }
          : { color: colorA1, x0: 0.0, x1: 23.0, y0: 0.0, y1: 127.0 }
          : { color: colorA7, x0: 0.0, x1: 40.0, y0: 327.0, y1: 343.0 }
          : { color: colorA0, x0: 190.0, x1: 203.0, y0: 0.0, y1: 118.0 }
          : { color: colorA5, x0: 84.0, x1: 189.0, y0: 210.0, y1: 224.0 }
          : { color: colorA7, x0: 239.0, x1: 323.0, y0: 422.0, y1: 433.0 }
          : { color: colorA6, x0: 347.0, x1: 456.0, y0: 119.0, y1: 130.0 }
          : { color: colorA5, x0: 162.0, x1: 195.0, y0: 390.0, y1: 421.0 }
          : { color: colorA2, x0: 24.0, x1: 40.0, y0: 0.0, y1: 30.0 }
          : Nil
      )
    +> V.empty

tiles2 :: V.Vec D16 (List TileBuilder)
tiles2 =
  ( { color: colorB3, x0: 300.0, x1: 511.0, y0: 340.0, y1: 511.0 }
      : { color: colorB7, x0: 319.0, x1: 511.0, y0: 167.0, y1: 339.0 }
      : { color: colorB7, x0: 131.0, x1: 511.0, y0: 0.0, y1: 166.0 }
      : { color: colorB5, x0: 0.0, x1: 130.0, y0: 0.0, y1: 192.0 }
      : { color: colorB2, x0: 250.0, x1: 318.0, y0: 167.0, y1: 205.0 }
      : { color: colorB6, x0: 72.0, x1: 240.0, y0: 333.0, y1: 511.0 }
      : { color: colorB7, x0: 0.0, x1: 249.0, y0: 193.0, y1: 332.0 }
      : { color: colorB2, x0: 0.0, x1: 56.0, y0: 333.0, y1: 511.0 }
      : { color: colorB2, x0: 57.0, x1: 71.0, y0: 333.0, y1: 511.0 }
      : { color: colorB5, x0: 131.0, x1: 249.0, y0: 167.0, y1: 192.0 }
      : { color: colorB7, x0: 250.0, x1: 318.0, y0: 206.0, y1: 339.0 }
      : { color: colorB6, x0: 241.0, x1: 299.0, y0: 340.0, y1: 511.0 }
      : { color: colorB4, x0: 241.0, x1: 249.0, y0: 333.0, y1: 339.0 }
      : Nil
  )
    +> ( { color: colorB4, x0: 57.0, x1: 209.0, y0: 211.0, y1: 331.0 }
          : { color: colorB4, x0: 210.0, x1: 404.0, y0: 155.0, y1: 265.0 }
          : { color: colorB2, x0: 323.0, x1: 379.0, y0: 266.0, y1: 412.0 }
          : { color: colorB3, x0: 194.0, x1: 338.0, y0: 0.0, y1: 143.0 }
          : { color: colorB5, x0: 210.0, x1: 248.0, y0: 266.0, y1: 312.0 }
          : { color: colorB1, x0: 39.0, x1: 89.0, y0: 332.0, y1: 511.0 }
          : { color: colorB5, x0: 341.0, x1: 511.0, y0: 413.0, y1: 511.0 }
          : { color: colorB5, x0: 122.0, x1: 182.0, y0: 0.0, y1: 107.0 }
          : { color: colorB0, x0: 275.0, x1: 322.0, y0: 291.0, y1: 441.0 }
          : { color: colorB4, x0: 0.0, x1: 56.0, y0: 162.0, y1: 266.0 }
          : { color: colorB7, x0: 380.0, x1: 511.0, y0: 266.0, y1: 412.0 }
          : { color: colorB1, x0: 0.0, x1: 38.0, y0: 267.0, y1: 511.0 }
          : { color: colorB0, x0: 183.0, x1: 193.0, y0: 0.0, y1: 210.0 }
          : { color: colorB7, x0: 429.0, x1: 489.0, y0: 111.0, y1: 151.0 }
          : { color: colorB5, x0: 249.0, x1: 322.0, y0: 266.0, y1: 290.0 }
          : { color: colorB3, x0: 210.0, x1: 274.0, y0: 313.0, y1: 405.0 }
          : { color: colorB7, x0: 376.0, x1: 412.0, y0: 0.0, y1: 60.0 }
          : { color: colorB7, x0: 27.0, x1: 121.0, y0: 31.0, y1: 107.0 }
          : { color: colorB4, x0: 57.0, x1: 182.0, y0: 108.0, y1: 210.0 }
          : { color: colorB4, x0: 90.0, x1: 114.0, y0: 369.0, y1: 511.0 }
          : { color: colorB7, x0: 435.0, x1: 511.0, y0: 152.0, y1: 265.0 }
          : { color: colorB4, x0: 115.0, x1: 185.0, y0: 401.0, y1: 511.0 }
          : { color: colorB3, x0: 90.0, x1: 209.0, y0: 332.0, y1: 368.0 }
          : { color: colorB0, x0: 339.0, x1: 375.0, y0: 0.0, y1: 154.0 }
          : { color: colorB0, x0: 186.0, x1: 340.0, y0: 442.0, y1: 511.0 }
          : { color: colorB6, x0: 175.0, x1: 209.0, y0: 369.0, y1: 400.0 }
          : { color: colorB5, x0: 115.0, x1: 174.0, y0: 369.0, y1: 400.0 }
          : { color: colorB6, x0: 376.0, x1: 403.0, y0: 99.0, y1: 135.0 }
          : { color: colorB6, x0: 0.0, x1: 56.0, y0: 108.0, y1: 161.0 }
          : { color: colorB2, x0: 39.0, x1: 56.0, y0: 267.0, y1: 331.0 }
          : { color: colorB1, x0: 249.0, x1: 274.0, y0: 291.0, y1: 312.0 }
          : { color: colorB4, x0: 194.0, x1: 338.0, y0: 144.0, y1: 154.0 }
          : { color: colorB5, x0: 204.0, x1: 274.0, y0: 406.0, y1: 441.0 }
          : { color: colorB4, x0: 0.0, x1: 121.0, y0: 0.0, y1: 30.0 }
          : { color: colorB5, x0: 449.0, x1: 511.0, y0: 60.0, y1: 88.0 }
          : { color: colorB5, x0: 413.0, x1: 511.0, y0: 0.0, y1: 59.0 }
          : { color: colorB3, x0: 405.0, x1: 434.0, y0: 184.0, y1: 238.0 }
          : { color: colorB0, x0: 413.0, x1: 448.0, y0: 60.0, y1: 110.0 }
          : { color: colorB0, x0: 449.0, x1: 511.0, y0: 89.0, y1: 110.0 }
          : { color: colorB1, x0: 323.0, x1: 340.0, y0: 413.0, y1: 441.0 }
          : { color: colorB1, x0: 186.0, x1: 203.0, y0: 401.0, y1: 441.0 }
          : { color: colorB2, x0: 0.0, x1: 26.0, y0: 31.0, y1: 107.0 }
          : { color: colorB3, x0: 405.0, x1: 428.0, y0: 111.0, y1: 183.0 }
          : { color: colorB4, x0: 405.0, x1: 434.0, y0: 239.0, y1: 265.0 }
          : { color: colorB4, x0: 490.0, x1: 511.0, y0: 111.0, y1: 151.0 }
          : { color: colorB4, x0: 376.0, x1: 412.0, y0: 61.0, y1: 98.0 }
          : { color: colorB4, x0: 376.0, x1: 404.0, y0: 136.0, y1: 154.0 }
          : { color: colorB6, x0: 429.0, x1: 434.0, y0: 152.0, y1: 183.0 }
          : { color: colorB5, x0: 194.0, x1: 209.0, y0: 155.0, y1: 210.0 }
          : { color: colorB2, x0: 404.0, x1: 412.0, y0: 99.0, y1: 110.0 }
          : { color: colorB4, x0: 204.0, x1: 209.0, y0: 401.0, y1: 405.0 }
          : { color: colorB3, x0: 404.0, x1: 404.0, y0: 111.0, y1: 135.0 }
          : Nil
      )
    +> ( { color: colorB2, x0: 202.0, x1: 442.0, y0: 0.0, y1: 156.0 }
          : { color: colorB1, x0: 134.0, x1: 511.0, y0: 352.0, y1: 511.0 }
          : { color: colorB4, x0: 20.0, x1: 260.0, y0: 255.0, y1: 325.0 }
          : { color: colorB1, x0: 261.0, x1: 351.0, y0: 229.0, y1: 343.0 }
          : { color: colorB2, x0: 90.0, x1: 133.0, y0: 436.0, y1: 496.0 }
          : { color: colorB2, x0: 0.0, x1: 64.0, y0: 146.0, y1: 202.0 }
          : { color: colorB0, x0: 0.0, x1: 118.0, y0: 0.0, y1: 97.0 }
          : { color: colorB0, x0: 0.0, x1: 89.0, y0: 326.0, y1: 511.0 }
          : { color: colorB5, x0: 443.0, x1: 511.0, y0: 0.0, y1: 351.0 }
          : { color: colorB5, x0: 65.0, x1: 213.0, y0: 172.0, y1: 254.0 }
          : { color: colorB2, x0: 0.0, x1: 201.0, y0: 98.0, y1: 145.0 }
          : { color: colorB3, x0: 352.0, x1: 442.0, y0: 157.0, y1: 351.0 }
          : { color: colorB7, x0: 90.0, x1: 133.0, y0: 326.0, y1: 435.0 }
          : { color: colorB3, x0: 65.0, x1: 201.0, y0: 146.0, y1: 171.0 }
          : { color: colorB2, x0: 214.0, x1: 260.0, y0: 157.0, y1: 254.0 }
          : { color: colorB6, x0: 261.0, x1: 351.0, y0: 157.0, y1: 228.0 }
          : { color: colorB6, x0: 0.0, x1: 64.0, y0: 203.0, y1: 254.0 }
          : { color: colorB5, x0: 143.0, x1: 167.0, y0: 0.0, y1: 68.0 }
          : { color: colorB5, x0: 119.0, x1: 201.0, y0: 69.0, y1: 97.0 }
          : { color: colorB4, x0: 134.0, x1: 260.0, y0: 326.0, y1: 351.0 }
          : { color: colorB4, x0: 261.0, x1: 351.0, y0: 344.0, y1: 351.0 }
          : { color: colorB0, x0: 168.0, x1: 201.0, y0: 0.0, y1: 68.0 }
          : { color: colorB5, x0: 90.0, x1: 133.0, y0: 497.0, y1: 511.0 }
          : { color: colorB2, x0: 0.0, x1: 19.0, y0: 255.0, y1: 325.0 }
          : { color: colorB7, x0: 119.0, x1: 142.0, y0: 0.0, y1: 68.0 }
          : { color: colorB5, x0: 202.0, x1: 213.0, y0: 157.0, y1: 171.0 }
          : Nil
      )
    +> ( { color: colorB5, x0: 453.0, x1: 511.0, y0: 291.0, y1: 375.0 }
          : { color: colorB1, x0: 145.0, x1: 329.0, y0: 257.0, y1: 433.0 }
          : { color: colorB3, x0: 0.0, x1: 234.0, y0: 0.0, y1: 243.0 }
          : { color: colorB4, x0: 348.0, x1: 452.0, y0: 207.0, y1: 391.0 }
          : { color: colorB7, x0: 49.0, x1: 103.0, y0: 445.0, y1: 511.0 }
          : { color: colorB7, x0: 0.0, x1: 347.0, y0: 244.0, y1: 256.0 }
          : { color: colorB7, x0: 104.0, x1: 379.0, y0: 434.0, y1: 511.0 }
          : { color: colorB3, x0: 453.0, x1: 511.0, y0: 244.0, y1: 290.0 }
          : { color: colorB0, x0: 235.0, x1: 262.0, y0: 145.0, y1: 243.0 }
          : { color: colorB5, x0: 315.0, x1: 439.0, y0: 167.0, y1: 206.0 }
          : { color: colorB4, x0: 0.0, x1: 99.0, y0: 257.0, y1: 347.0 }
          : { color: colorB7, x0: 380.0, x1: 465.0, y0: 465.0, y1: 511.0 }
          : { color: colorB5, x0: 380.0, x1: 511.0, y0: 392.0, y1: 464.0 }
          : { color: colorB2, x0: 392.0, x1: 511.0, y0: 44.0, y1: 166.0 }
          : { color: colorB1, x0: 100.0, x1: 144.0, y0: 257.0, y1: 433.0 }
          : { color: colorB3, x0: 0.0, x1: 99.0, y0: 348.0, y1: 444.0 }
          : { color: colorB2, x0: 235.0, x1: 391.0, y0: 0.0, y1: 144.0 }
          : { color: colorB4, x0: 440.0, x1: 511.0, y0: 167.0, y1: 206.0 }
          : { color: colorB7, x0: 453.0, x1: 511.0, y0: 376.0, y1: 391.0 }
          : { color: colorB0, x0: 263.0, x1: 314.0, y0: 145.0, y1: 243.0 }
          : { color: colorB3, x0: 392.0, x1: 460.0, y0: 0.0, y1: 43.0 }
          : { color: colorB6, x0: 466.0, x1: 511.0, y0: 474.0, y1: 511.0 }
          : { color: colorB6, x0: 0.0, x1: 48.0, y0: 445.0, y1: 511.0 }
          : { color: colorB7, x0: 461.0, x1: 511.0, y0: 0.0, y1: 43.0 }
          : { color: colorB2, x0: 315.0, x1: 347.0, y0: 207.0, y1: 243.0 }
          : { color: colorB0, x0: 330.0, x1: 347.0, y0: 257.0, y1: 433.0 }
          : { color: colorB3, x0: 453.0, x1: 511.0, y0: 207.0, y1: 243.0 }
          : { color: colorB3, x0: 315.0, x1: 391.0, y0: 145.0, y1: 166.0 }
          : { color: colorB5, x0: 348.0, x1: 379.0, y0: 392.0, y1: 433.0 }
          : { color: colorB0, x0: 466.0, x1: 511.0, y0: 465.0, y1: 473.0 }
          : { color: colorB0, x0: 100.0, x1: 103.0, y0: 434.0, y1: 444.0 }
          : Nil
      )
    +> ( { color: colorB7, x0: 139.0, x1: 235.0, y0: 0.0, y1: 236.0 }
          : { color: colorB4, x0: 0.0, x1: 97.0, y0: 174.0, y1: 264.0 }
          : { color: colorB0, x0: 98.0, x1: 160.0, y0: 237.0, y1: 291.0 }
          : { color: colorB6, x0: 0.0, x1: 97.0, y0: 265.0, y1: 511.0 }
          : { color: colorB4, x0: 395.0, x1: 451.0, y0: 449.0, y1: 461.0 }
          : { color: colorB1, x0: 0.0, x1: 138.0, y0: 0.0, y1: 125.0 }
          : { color: colorB6, x0: 469.0, x1: 481.0, y0: 432.0, y1: 484.0 }
          : { color: colorB2, x0: 98.0, x1: 286.0, y0: 303.0, y1: 463.0 }
          : { color: colorB7, x0: 287.0, x1: 368.0, y0: 0.0, y1: 384.0 }
          : { color: colorB5, x0: 236.0, x1: 286.0, y0: 17.0, y1: 249.0 }
          : { color: colorB7, x0: 369.0, x1: 511.0, y0: 48.0, y1: 312.0 }
          : { color: colorB1, x0: 482.0, x1: 511.0, y0: 313.0, y1: 511.0 }
          : { color: colorB2, x0: 337.0, x1: 468.0, y0: 462.0, y1: 511.0 }
          : { color: colorB5, x0: 161.0, x1: 235.0, y0: 237.0, y1: 302.0 }
          : { color: colorB7, x0: 369.0, x1: 481.0, y0: 313.0, y1: 431.0 }
          : { color: colorB1, x0: 297.0, x1: 394.0, y0: 432.0, y1: 461.0 }
          : { color: colorB1, x0: 0.0, x1: 138.0, y0: 126.0, y1: 173.0 }
          : { color: colorB6, x0: 98.0, x1: 336.0, y0: 464.0, y1: 511.0 }
          : { color: colorB1, x0: 236.0, x1: 286.0, y0: 250.0, y1: 302.0 }
          : { color: colorB0, x0: 287.0, x1: 368.0, y0: 385.0, y1: 431.0 }
          : { color: colorB0, x0: 437.0, x1: 493.0, y0: 0.0, y1: 47.0 }
          : { color: colorB2, x0: 98.0, x1: 138.0, y0: 174.0, y1: 236.0 }
          : { color: colorB6, x0: 494.0, x1: 511.0, y0: 0.0, y1: 47.0 }
          : { color: colorB5, x0: 395.0, x1: 468.0, y0: 432.0, y1: 448.0 }
          : { color: colorB2, x0: 369.0, x1: 436.0, y0: 0.0, y1: 27.0 }
          : { color: colorB5, x0: 452.0, x1: 468.0, y0: 449.0, y1: 461.0 }
          : { color: colorB5, x0: 236.0, x1: 286.0, y0: 0.0, y1: 16.0 }
          : { color: colorB0, x0: 369.0, x1: 436.0, y0: 28.0, y1: 47.0 }
          : { color: colorB1, x0: 469.0, x1: 481.0, y0: 485.0, y1: 511.0 }
          : { color: colorB1, x0: 287.0, x1: 296.0, y0: 432.0, y1: 463.0 }
          : { color: colorB3, x0: 297.0, x1: 336.0, y0: 462.0, y1: 463.0 }
          : { color: colorB0, x0: 98.0, x1: 160.0, y0: 292.0, y1: 302.0 }
          : Nil
      )
    +> ( { color: colorB4, x0: 337.0, x1: 413.0, y0: 339.0, y1: 451.0 }
          : { color: colorB4, x0: 454.0, x1: 511.0, y0: 0.0, y1: 258.0 }
          : { color: colorB3, x0: 255.0, x1: 453.0, y0: 134.0, y1: 278.0 }
          : { color: colorB5, x0: 423.0, x1: 503.0, y0: 279.0, y1: 428.0 }
          : { color: colorB3, x0: 129.0, x1: 201.0, y0: 0.0, y1: 34.0 }
          : { color: colorB0, x0: 504.0, x1: 511.0, y0: 259.0, y1: 511.0 }
          : { color: colorB1, x0: 269.0, x1: 377.0, y0: 279.0, y1: 338.0 }
          : { color: colorB6, x0: 0.0, x1: 125.0, y0: 280.0, y1: 511.0 }
          : { color: colorB7, x0: 202.0, x1: 453.0, y0: 0.0, y1: 133.0 }
          : { color: colorB5, x0: 243.0, x1: 327.0, y0: 422.0, y1: 511.0 }
          : { color: colorB7, x0: 141.0, x1: 201.0, y0: 371.0, y1: 499.0 }
          : { color: colorB1, x0: 42.0, x1: 186.0, y0: 87.0, y1: 225.0 }
          : { color: colorB3, x0: 220.0, x1: 254.0, y0: 134.0, y1: 191.0 }
          : { color: colorB2, x0: 187.0, x1: 254.0, y0: 192.0, y1: 370.0 }
          : { color: colorB1, x0: 0.0, x1: 128.0, y0: 0.0, y1: 86.0 }
          : { color: colorB0, x0: 119.0, x1: 186.0, y0: 226.0, y1: 279.0 }
          : { color: colorB7, x0: 202.0, x1: 336.0, y0: 371.0, y1: 421.0 }
          : { color: colorB2, x0: 414.0, x1: 503.0, y0: 429.0, y1: 511.0 }
          : { color: colorB7, x0: 27.0, x1: 118.0, y0: 226.0, y1: 279.0 }
          : { color: colorB0, x0: 126.0, x1: 140.0, y0: 280.0, y1: 511.0 }
          : { color: colorB7, x0: 378.0, x1: 422.0, y0: 279.0, y1: 338.0 }
          : { color: colorB2, x0: 255.0, x1: 268.0, y0: 279.0, y1: 370.0 }
          : { color: colorB4, x0: 454.0, x1: 503.0, y0: 259.0, y1: 278.0 }
          : { color: colorB2, x0: 0.0, x1: 41.0, y0: 87.0, y1: 225.0 }
          : { color: colorB0, x0: 129.0, x1: 201.0, y0: 35.0, y1: 86.0 }
          : { color: colorB6, x0: 141.0, x1: 186.0, y0: 280.0, y1: 362.0 }
          : { color: colorB5, x0: 328.0, x1: 413.0, y0: 452.0, y1: 511.0 }
          : { color: colorB2, x0: 141.0, x1: 242.0, y0: 500.0, y1: 511.0 }
          : { color: colorB3, x0: 269.0, x1: 336.0, y0: 339.0, y1: 370.0 }
          : { color: colorB7, x0: 187.0, x1: 201.0, y0: 87.0, y1: 191.0 }
          : { color: colorB4, x0: 202.0, x1: 242.0, y0: 422.0, y1: 499.0 }
          : { color: colorB6, x0: 0.0, x1: 26.0, y0: 226.0, y1: 279.0 }
          : { color: colorB6, x0: 414.0, x1: 422.0, y0: 339.0, y1: 428.0 }
          : { color: colorB5, x0: 202.0, x1: 219.0, y0: 134.0, y1: 191.0 }
          : { color: colorB2, x0: 141.0, x1: 186.0, y0: 363.0, y1: 370.0 }
          : { color: colorB5, x0: 328.0, x1: 336.0, y0: 422.0, y1: 451.0 }
          : Nil
      )
    +> ( { color: colorB3, x0: 22.0, x1: 82.0, y0: 0.0, y1: 356.0 }
          : { color: colorB5, x0: 83.0, x1: 259.0, y0: 0.0, y1: 144.0 }
          : { color: colorB6, x0: 168.0, x1: 360.0, y0: 154.0, y1: 280.0 }
          : { color: colorB2, x0: 260.0, x1: 434.0, y0: 0.0, y1: 90.0 }
          : { color: colorB7, x0: 208.0, x1: 432.0, y0: 281.0, y1: 368.0 }
          : { color: colorB0, x0: 103.0, x1: 319.0, y0: 369.0, y1: 511.0 }
          : { color: colorB1, x0: 433.0, x1: 511.0, y0: 254.0, y1: 410.0 }
          : { color: colorB0, x0: 83.0, x1: 167.0, y0: 145.0, y1: 368.0 }
          : { color: colorB4, x0: 0.0, x1: 21.0, y0: 62.0, y1: 172.0 }
          : { color: colorB0, x0: 361.0, x1: 511.0, y0: 91.0, y1: 226.0 }
          : { color: colorB0, x0: 335.0, x1: 361.0, y0: 491.0, y1: 511.0 }
          : { color: colorB6, x0: 361.0, x1: 511.0, y0: 227.0, y1: 253.0 }
          : { color: colorB5, x0: 0.0, x1: 82.0, y0: 357.0, y1: 511.0 }
          : { color: colorB6, x0: 320.0, x1: 411.0, y0: 436.0, y1: 490.0 }
          : { color: colorB6, x0: 412.0, x1: 511.0, y0: 411.0, y1: 511.0 }
          : { color: colorB3, x0: 83.0, x1: 102.0, y0: 369.0, y1: 511.0 }
          : { color: colorB2, x0: 260.0, x1: 300.0, y0: 91.0, y1: 153.0 }
          : { color: colorB5, x0: 301.0, x1: 360.0, y0: 91.0, y1: 153.0 }
          : { color: colorB4, x0: 0.0, x1: 21.0, y0: 0.0, y1: 61.0 }
          : { color: colorB5, x0: 435.0, x1: 511.0, y0: 0.0, y1: 90.0 }
          : { color: colorB4, x0: 361.0, x1: 432.0, y0: 254.0, y1: 280.0 }
          : { color: colorB5, x0: 320.0, x1: 411.0, y0: 369.0, y1: 435.0 }
          : { color: colorB0, x0: 412.0, x1: 432.0, y0: 369.0, y1: 410.0 }
          : { color: colorB7, x0: 168.0, x1: 207.0, y0: 281.0, y1: 315.0 }
          : { color: colorB5, x0: 168.0, x1: 207.0, y0: 316.0, y1: 368.0 }
          : { color: colorB3, x0: 362.0, x1: 411.0, y0: 491.0, y1: 511.0 }
          : { color: colorB3, x0: 0.0, x1: 21.0, y0: 173.0, y1: 356.0 }
          : { color: colorB0, x0: 168.0, x1: 259.0, y0: 145.0, y1: 153.0 }
          : { color: colorB2, x0: 320.0, x1: 334.0, y0: 491.0, y1: 511.0 }
          : Nil
      )
    +> ( { color: colorB2, x0: 204.0, x1: 276.0, y0: 418.0, y1: 496.0 }
          : { color: colorB2, x0: 277.0, x1: 443.0, y0: 364.0, y1: 494.0 }
          : { color: colorB5, x0: 78.0, x1: 262.0, y0: 211.0, y1: 307.0 }
          : { color: colorB1, x0: 162.0, x1: 234.0, y0: 66.0, y1: 158.0 }
          : { color: colorB5, x0: 429.0, x1: 499.0, y0: 46.0, y1: 144.0 }
          : { color: colorB4, x0: 296.0, x1: 350.0, y0: 161.0, y1: 203.0 }
          : { color: colorB5, x0: 29.0, x1: 133.0, y0: 94.0, y1: 150.0 }
          : { color: colorB2, x0: 386.0, x1: 434.0, y0: 293.0, y1: 355.0 }
          : { color: colorB4, x0: 263.0, x1: 511.0, y0: 204.0, y1: 292.0 }
          : { color: colorB1, x0: 0.0, x1: 276.0, y0: 308.0, y1: 417.0 }
          : { color: colorB6, x0: 0.0, x1: 136.0, y0: 467.0, y1: 511.0 }
          : { color: colorB2, x0: 0.0, x1: 203.0, y0: 418.0, y1: 466.0 }
          : { color: colorB5, x0: 131.0, x1: 511.0, y0: 0.0, y1: 45.0 }
          : { color: colorB2, x0: 0.0, x1: 130.0, y0: 0.0, y1: 93.0 }
          : { color: colorB5, x0: 131.0, x1: 161.0, y0: 46.0, y1: 93.0 }
          : { color: colorB5, x0: 291.0, x1: 453.0, y0: 495.0, y1: 511.0 }
          : { color: colorB4, x0: 0.0, x1: 77.0, y0: 151.0, y1: 307.0 }
          : { color: colorB4, x0: 78.0, x1: 262.0, y0: 159.0, y1: 210.0 }
          : { color: colorB5, x0: 263.0, x1: 387.0, y0: 70.0, y1: 160.0 }
          : { color: colorB1, x0: 351.0, x1: 511.0, y0: 161.0, y1: 203.0 }
          : { color: colorB6, x0: 388.0, x1: 428.0, y0: 46.0, y1: 160.0 }
          : { color: colorB2, x0: 454.0, x1: 511.0, y0: 469.0, y1: 511.0 }
          : { color: colorB3, x0: 235.0, x1: 262.0, y0: 46.0, y1: 158.0 }
          : { color: colorB0, x0: 137.0, x1: 203.0, y0: 467.0, y1: 511.0 }
          : { color: colorB0, x0: 435.0, x1: 489.0, y0: 293.0, y1: 352.0 }
          : { color: colorB6, x0: 162.0, x1: 234.0, y0: 46.0, y1: 65.0 }
          : { color: colorB6, x0: 348.0, x1: 385.0, y0: 293.0, y1: 363.0 }
          : { color: colorB4, x0: 490.0, x1: 511.0, y0: 293.0, y1: 468.0 }
          : { color: colorB0, x0: 263.0, x1: 347.0, y0: 293.0, y1: 307.0 }
          : { color: colorB6, x0: 429.0, x1: 511.0, y0: 145.0, y1: 160.0 }
          : { color: colorB0, x0: 204.0, x1: 290.0, y0: 497.0, y1: 511.0 }
          : { color: colorB3, x0: 0.0, x1: 28.0, y0: 94.0, y1: 150.0 }
          : { color: colorB2, x0: 277.0, x1: 347.0, y0: 308.0, y1: 363.0 }
          : { color: colorB1, x0: 444.0, x1: 489.0, y0: 353.0, y1: 468.0 }
          : { color: colorB3, x0: 500.0, x1: 511.0, y0: 46.0, y1: 144.0 }
          : { color: colorB6, x0: 263.0, x1: 295.0, y0: 161.0, y1: 203.0 }
          : { color: colorB2, x0: 263.0, x1: 387.0, y0: 46.0, y1: 69.0 }
          : { color: colorB0, x0: 134.0, x1: 161.0, y0: 94.0, y1: 158.0 }
          : { color: colorB3, x0: 78.0, x1: 133.0, y0: 151.0, y1: 158.0 }
          : { color: colorB7, x0: 444.0, x1: 453.0, y0: 469.0, y1: 494.0 }
          : { color: colorB5, x0: 386.0, x1: 443.0, y0: 356.0, y1: 363.0 }
          : { color: colorB5, x0: 277.0, x1: 290.0, y0: 495.0, y1: 496.0 }
          : { color: colorB6, x0: 435.0, x1: 443.0, y0: 353.0, y1: 355.0 }
          : Nil
      )
    +> ( { color: colorB7, x0: 394.0, x1: 464.0, y0: 120.0, y1: 344.0 }
          : { color: colorB5, x0: 222.0, x1: 288.0, y0: 95.0, y1: 269.0 }
          : { color: colorB1, x0: 307.0, x1: 393.0, y0: 142.0, y1: 398.0 }
          : { color: colorB0, x0: 109.0, x1: 155.0, y0: 264.0, y1: 318.0 }
          : { color: colorB3, x0: 0.0, x1: 108.0, y0: 174.0, y1: 318.0 }
          : { color: colorB4, x0: 126.0, x1: 346.0, y0: 0.0, y1: 65.0 }
          : { color: colorB7, x0: 341.0, x1: 429.0, y0: 399.0, y1: 511.0 }
          : { color: colorB0, x0: 109.0, x1: 204.0, y0: 66.0, y1: 263.0 }
          : { color: colorB0, x0: 42.0, x1: 74.0, y0: 37.0, y1: 105.0 }
          : { color: colorB1, x0: 0.0, x1: 125.0, y0: 0.0, y1: 36.0 }
          : { color: colorB6, x0: 42.0, x1: 170.0, y0: 331.0, y1: 443.0 }
          : { color: colorB5, x0: 214.0, x1: 256.0, y0: 484.0, y1: 511.0 }
          : { color: colorB7, x0: 171.0, x1: 306.0, y0: 270.0, y1: 483.0 }
          : { color: colorB6, x0: 0.0, x1: 108.0, y0: 106.0, y1: 173.0 }
          : { color: colorB3, x0: 336.0, x1: 393.0, y0: 66.0, y1: 141.0 }
          : { color: colorB0, x0: 205.0, x1: 221.0, y0: 66.0, y1: 269.0 }
          : { color: colorB3, x0: 430.0, x1: 511.0, y0: 345.0, y1: 511.0 }
          : { color: colorB4, x0: 394.0, x1: 429.0, y0: 345.0, y1: 398.0 }
          : { color: colorB5, x0: 43.0, x1: 170.0, y0: 444.0, y1: 511.0 }
          : { color: colorB6, x0: 0.0, x1: 170.0, y0: 319.0, y1: 330.0 }
          : { color: colorB0, x0: 372.0, x1: 511.0, y0: 0.0, y1: 44.0 }
          : { color: colorB2, x0: 0.0, x1: 41.0, y0: 331.0, y1: 511.0 }
          : { color: colorB1, x0: 465.0, x1: 511.0, y0: 96.0, y1: 132.0 }
          : { color: colorB6, x0: 75.0, x1: 125.0, y0: 37.0, y1: 65.0 }
          : { color: colorB2, x0: 0.0, x1: 41.0, y0: 37.0, y1: 105.0 }
          : { color: colorB2, x0: 222.0, x1: 335.0, y0: 66.0, y1: 94.0 }
          : { color: colorB1, x0: 75.0, x1: 108.0, y0: 66.0, y1: 105.0 }
          : { color: colorB1, x0: 394.0, x1: 511.0, y0: 45.0, y1: 95.0 }
          : { color: colorB7, x0: 465.0, x1: 511.0, y0: 133.0, y1: 344.0 }
          : { color: colorB2, x0: 257.0, x1: 340.0, y0: 484.0, y1: 511.0 }
          : { color: colorB6, x0: 156.0, x1: 170.0, y0: 264.0, y1: 318.0 }
          : { color: colorB0, x0: 171.0, x1: 213.0, y0: 484.0, y1: 511.0 }
          : { color: colorB6, x0: 289.0, x1: 335.0, y0: 95.0, y1: 141.0 }
          : { color: colorB6, x0: 394.0, x1: 464.0, y0: 96.0, y1: 119.0 }
          : { color: colorB4, x0: 289.0, x1: 306.0, y0: 201.0, y1: 249.0 }
          : { color: colorB0, x0: 307.0, x1: 340.0, y0: 399.0, y1: 483.0 }
          : { color: colorB2, x0: 347.0, x1: 393.0, y0: 45.0, y1: 65.0 }
          : { color: colorB7, x0: 171.0, x1: 204.0, y0: 264.0, y1: 269.0 }
          : { color: colorB1, x0: 289.0, x1: 306.0, y0: 142.0, y1: 200.0 }
          : { color: colorB0, x0: 289.0, x1: 306.0, y0: 250.0, y1: 269.0 }
          : { color: colorB2, x0: 347.0, x1: 371.0, y0: 0.0, y1: 44.0 }
          : { color: colorB4, x0: 42.0, x1: 42.0, y0: 444.0, y1: 511.0 }
          : Nil
      )
    +> ( { color: colorB5, x0: 70.0, x1: 142.0, y0: 0.0, y1: 64.0 }
          : { color: colorB4, x0: 416.0, x1: 511.0, y0: 258.0, y1: 386.0 }
          : { color: colorB3, x0: 149.0, x1: 349.0, y0: 386.0, y1: 511.0 }
          : { color: colorB2, x0: 350.0, x1: 474.0, y0: 430.0, y1: 511.0 }
          : { color: colorB7, x0: 91.0, x1: 259.0, y0: 86.0, y1: 230.0 }
          : { color: colorB3, x0: 391.0, x1: 511.0, y0: 387.0, y1: 429.0 }
          : { color: colorB2, x0: 278.0, x1: 324.0, y0: 208.0, y1: 254.0 }
          : { color: colorB7, x0: 220.0, x1: 400.0, y0: 255.0, y1: 385.0 }
          : { color: colorB2, x0: 45.0, x1: 115.0, y0: 347.0, y1: 511.0 }
          : { color: colorB4, x0: 475.0, x1: 511.0, y0: 430.0, y1: 511.0 }
          : { color: colorB2, x0: 325.0, x1: 511.0, y0: 138.0, y1: 246.0 }
          : { color: colorB0, x0: 116.0, x1: 148.0, y0: 231.0, y1: 511.0 }
          : { color: colorB1, x0: 260.0, x1: 511.0, y0: 0.0, y1: 127.0 }
          : { color: colorB0, x0: 0.0, x1: 115.0, y0: 231.0, y1: 346.0 }
          : { color: colorB3, x0: 143.0, x1: 259.0, y0: 0.0, y1: 85.0 }
          : { color: colorB2, x0: 280.0, x1: 296.0, y0: 128.0, y1: 174.0 }
          : { color: colorB3, x0: 0.0, x1: 142.0, y0: 65.0, y1: 85.0 }
          : { color: colorB4, x0: 401.0, x1: 415.0, y0: 247.0, y1: 386.0 }
          : { color: colorB6, x0: 297.0, x1: 511.0, y0: 128.0, y1: 137.0 }
          : { color: colorB6, x0: 0.0, x1: 44.0, y0: 347.0, y1: 511.0 }
          : { color: colorB4, x0: 149.0, x1: 219.0, y0: 231.0, y1: 385.0 }
          : { color: colorB5, x0: 0.0, x1: 90.0, y0: 86.0, y1: 230.0 }
          : { color: colorB3, x0: 350.0, x1: 390.0, y0: 386.0, y1: 429.0 }
          : { color: colorB6, x0: 0.0, x1: 69.0, y0: 0.0, y1: 64.0 }
          : { color: colorB4, x0: 260.0, x1: 279.0, y0: 128.0, y1: 207.0 }
          : { color: colorB1, x0: 416.0, x1: 511.0, y0: 247.0, y1: 257.0 }
          : { color: colorB2, x0: 325.0, x1: 400.0, y0: 247.0, y1: 254.0 }
          : { color: colorB7, x0: 260.0, x1: 277.0, y0: 208.0, y1: 254.0 }
          : { color: colorB4, x0: 297.0, x1: 324.0, y0: 138.0, y1: 207.0 }
          : { color: colorB2, x0: 220.0, x1: 259.0, y0: 231.0, y1: 254.0 }
          : { color: colorB4, x0: 280.0, x1: 296.0, y0: 175.0, y1: 207.0 }
          : { color: colorB0, x0: 391.0, x1: 400.0, y0: 386.0, y1: 386.0 }
          : Nil
      )
    +> ( { color: colorB5, x0: 259.0, x1: 329.0, y0: 189.0, y1: 399.0 }
          : { color: colorB6, x0: 250.0, x1: 370.0, y0: 400.0, y1: 511.0 }
          : { color: colorB5, x0: 104.0, x1: 249.0, y0: 298.0, y1: 438.0 }
          : { color: colorB6, x0: 0.0, x1: 29.0, y0: 0.0, y1: 290.0 }
          : { color: colorB1, x0: 195.0, x1: 235.0, y0: 26.0, y1: 297.0 }
          : { color: colorB5, x0: 471.0, x1: 503.0, y0: 136.0, y1: 184.0 }
          : { color: colorB1, x0: 473.0, x1: 511.0, y0: 51.0, y1: 135.0 }
          : { color: colorB7, x0: 285.0, x1: 357.0, y0: 0.0, y1: 131.0 }
          : { color: colorB3, x0: 115.0, x1: 194.0, y0: 126.0, y1: 297.0 }
          : { color: colorB3, x0: 69.0, x1: 103.0, y0: 262.0, y1: 470.0 }
          : { color: colorB5, x0: 371.0, x1: 511.0, y0: 352.0, y1: 511.0 }
          : { color: colorB3, x0: 30.0, x1: 114.0, y0: 28.0, y1: 188.0 }
          : { color: colorB7, x0: 275.0, x1: 451.0, y0: 132.0, y1: 188.0 }
          : { color: colorB5, x0: 330.0, x1: 511.0, y0: 189.0, y1: 351.0 }
          : { color: colorB0, x0: 358.0, x1: 388.0, y0: 22.0, y1: 131.0 }
          : { color: colorB0, x0: 104.0, x1: 114.0, y0: 189.0, y1: 297.0 }
          : { color: colorB3, x0: 104.0, x1: 211.0, y0: 439.0, y1: 494.0 }
          : { color: colorB7, x0: 115.0, x1: 194.0, y0: 0.0, y1: 125.0 }
          : { color: colorB0, x0: 389.0, x1: 472.0, y0: 0.0, y1: 131.0 }
          : { color: colorB5, x0: 236.0, x1: 274.0, y0: 0.0, y1: 188.0 }
          : { color: colorB2, x0: 0.0, x1: 59.0, y0: 374.0, y1: 502.0 }
          : { color: colorB4, x0: 236.0, x1: 258.0, y0: 189.0, y1: 297.0 }
          : { color: colorB5, x0: 0.0, x1: 68.0, y0: 291.0, y1: 373.0 }
          : { color: colorB7, x0: 330.0, x1: 370.0, y0: 352.0, y1: 399.0 }
          : { color: colorB1, x0: 30.0, x1: 103.0, y0: 189.0, y1: 261.0 }
          : { color: colorB4, x0: 60.0, x1: 249.0, y0: 495.0, y1: 511.0 }
          : { color: colorB7, x0: 250.0, x1: 258.0, y0: 298.0, y1: 399.0 }
          : { color: colorB7, x0: 212.0, x1: 249.0, y0: 439.0, y1: 494.0 }
          : { color: colorB2, x0: 30.0, x1: 114.0, y0: 0.0, y1: 27.0 }
          : { color: colorB6, x0: 195.0, x1: 235.0, y0: 0.0, y1: 25.0 }
          : { color: colorB7, x0: 275.0, x1: 284.0, y0: 0.0, y1: 131.0 }
          : { color: colorB4, x0: 60.0, x1: 68.0, y0: 374.0, y1: 414.0 }
          : { color: colorB4, x0: 473.0, x1: 511.0, y0: 0.0, y1: 50.0 }
          : { color: colorB4, x0: 60.0, x1: 103.0, y0: 471.0, y1: 494.0 }
          : { color: colorB2, x0: 452.0, x1: 470.0, y0: 132.0, y1: 188.0 }
          : { color: colorB0, x0: 60.0, x1: 68.0, y0: 415.0, y1: 470.0 }
          : { color: colorB4, x0: 0.0, x1: 59.0, y0: 503.0, y1: 511.0 }
          : { color: colorB0, x0: 358.0, x1: 388.0, y0: 0.0, y1: 21.0 }
          : { color: colorB6, x0: 30.0, x1: 68.0, y0: 262.0, y1: 290.0 }
          : { color: colorB1, x0: 504.0, x1: 511.0, y0: 136.0, y1: 188.0 }
          : { color: colorB5, x0: 471.0, x1: 503.0, y0: 185.0, y1: 188.0 }
          : { color: colorB6, x0: 471.0, x1: 472.0, y0: 132.0, y1: 135.0 }
          : Nil
      )
    +> ( { color: colorB4, x0: 458.0, x1: 511.0, y0: 49.0, y1: 89.0 }
          : { color: colorB7, x0: 168.0, x1: 492.0, y0: 345.0, y1: 511.0 }
          : { color: colorB1, x0: 0.0, x1: 202.0, y0: 0.0, y1: 111.0 }
          : { color: colorB1, x0: 197.0, x1: 321.0, y0: 112.0, y1: 204.0 }
          : { color: colorB3, x0: 0.0, x1: 196.0, y0: 112.0, y1: 327.0 }
          : { color: colorB4, x0: 0.0, x1: 167.0, y0: 328.0, y1: 511.0 }
          : { color: colorB1, x0: 322.0, x1: 511.0, y0: 90.0, y1: 344.0 }
          : { color: colorB4, x0: 203.0, x1: 457.0, y0: 0.0, y1: 89.0 }
          : { color: colorB2, x0: 197.0, x1: 321.0, y0: 205.0, y1: 344.0 }
          : { color: colorB3, x0: 458.0, x1: 511.0, y0: 0.0, y1: 48.0 }
          : { color: colorB1, x0: 493.0, x1: 511.0, y0: 345.0, y1: 511.0 }
          : { color: colorB3, x0: 203.0, x1: 321.0, y0: 90.0, y1: 111.0 }
          : { color: colorB6, x0: 168.0, x1: 196.0, y0: 328.0, y1: 344.0 }
          : Nil
      )
    +> ( { color: colorB4, x0: 78.0, x1: 240.0, y0: 269.0, y1: 443.0 }
          : { color: colorB4, x0: 213.0, x1: 453.0, y0: 0.0, y1: 114.0 }
          : { color: colorB0, x0: 17.0, x1: 365.0, y0: 444.0, y1: 511.0 }
          : { color: colorB4, x0: 0.0, x1: 212.0, y0: 0.0, y1: 181.0 }
          : { color: colorB1, x0: 366.0, x1: 486.0, y0: 115.0, y1: 339.0 }
          : { color: colorB5, x0: 0.0, x1: 77.0, y0: 182.0, y1: 443.0 }
          : { color: colorB7, x0: 312.0, x1: 365.0, y0: 131.0, y1: 231.0 }
          : { color: colorB4, x0: 147.0, x1: 183.0, y0: 211.0, y1: 243.0 }
          : { color: colorB5, x0: 241.0, x1: 311.0, y0: 115.0, y1: 443.0 }
          : { color: colorB5, x0: 366.0, x1: 511.0, y0: 372.0, y1: 511.0 }
          : { color: colorB7, x0: 487.0, x1: 511.0, y0: 0.0, y1: 371.0 }
          : { color: colorB4, x0: 78.0, x1: 146.0, y0: 182.0, y1: 268.0 }
          : { color: colorB3, x0: 312.0, x1: 486.0, y0: 340.0, y1: 371.0 }
          : { color: colorB3, x0: 323.0, x1: 365.0, y0: 372.0, y1: 443.0 }
          : { color: colorB3, x0: 184.0, x1: 240.0, y0: 182.0, y1: 268.0 }
          : { color: colorB1, x0: 213.0, x1: 240.0, y0: 115.0, y1: 181.0 }
          : { color: colorB3, x0: 312.0, x1: 322.0, y0: 372.0, y1: 443.0 }
          : { color: colorB3, x0: 312.0, x1: 365.0, y0: 232.0, y1: 339.0 }
          : { color: colorB7, x0: 454.0, x1: 486.0, y0: 0.0, y1: 114.0 }
          : { color: colorB2, x0: 147.0, x1: 183.0, y0: 182.0, y1: 210.0 }
          : { color: colorB6, x0: 147.0, x1: 183.0, y0: 244.0, y1: 268.0 }
          : { color: colorB3, x0: 312.0, x1: 365.0, y0: 115.0, y1: 130.0 }
          : { color: colorB0, x0: 0.0, x1: 16.0, y0: 444.0, y1: 511.0 }
          : Nil
      )
    +> ( { color: colorB2, x0: 288.0, x1: 408.0, y0: 0.0, y1: 511.0 }
          : { color: colorB1, x0: 409.0, x1: 511.0, y0: 43.0, y1: 169.0 }
          : { color: colorB6, x0: 0.0, x1: 132.0, y0: 314.0, y1: 498.0 }
          : { color: colorB4, x0: 172.0, x1: 287.0, y0: 215.0, y1: 269.0 }
          : { color: colorB1, x0: 409.0, x1: 511.0, y0: 277.0, y1: 511.0 }
          : { color: colorB3, x0: 182.0, x1: 287.0, y0: 0.0, y1: 214.0 }
          : { color: colorB1, x0: 0.0, x1: 119.0, y0: 121.0, y1: 201.0 }
          : { color: colorB0, x0: 58.0, x1: 112.0, y0: 0.0, y1: 120.0 }
          : { color: colorB0, x0: 96.0, x1: 156.0, y0: 202.0, y1: 313.0 }
          : { color: colorB0, x0: 480.0, x1: 511.0, y0: 170.0, y1: 276.0 }
          : { color: colorB6, x0: 0.0, x1: 95.0, y0: 202.0, y1: 313.0 }
          : { color: colorB0, x0: 113.0, x1: 181.0, y0: 0.0, y1: 120.0 }
          : { color: colorB2, x0: 0.0, x1: 272.0, y0: 499.0, y1: 511.0 }
          : { color: colorB2, x0: 120.0, x1: 181.0, y0: 121.0, y1: 201.0 }
          : { color: colorB4, x0: 191.0, x1: 249.0, y0: 446.0, y1: 486.0 }
          : { color: colorB4, x0: 145.0, x1: 229.0, y0: 376.0, y1: 445.0 }
          : { color: colorB2, x0: 0.0, x1: 57.0, y0: 0.0, y1: 120.0 }
          : { color: colorB3, x0: 230.0, x1: 287.0, y0: 270.0, y1: 445.0 }
          : { color: colorB4, x0: 133.0, x1: 229.0, y0: 314.0, y1: 375.0 }
          : { color: colorB5, x0: 409.0, x1: 511.0, y0: 0.0, y1: 42.0 }
          : { color: colorB1, x0: 157.0, x1: 229.0, y0: 270.0, y1: 313.0 }
          : { color: colorB5, x0: 133.0, x1: 190.0, y0: 446.0, y1: 498.0 }
          : { color: colorB2, x0: 409.0, x1: 479.0, y0: 221.0, y1: 276.0 }
          : { color: colorB7, x0: 191.0, x1: 287.0, y0: 487.0, y1: 498.0 }
          : { color: colorB2, x0: 409.0, x1: 479.0, y0: 170.0, y1: 220.0 }
          : { color: colorB7, x0: 273.0, x1: 287.0, y0: 499.0, y1: 511.0 }
          : { color: colorB7, x0: 157.0, x1: 171.0, y0: 202.0, y1: 269.0 }
          : { color: colorB7, x0: 250.0, x1: 287.0, y0: 446.0, y1: 486.0 }
          : { color: colorB0, x0: 133.0, x1: 144.0, y0: 376.0, y1: 445.0 }
          : { color: colorB7, x0: 172.0, x1: 181.0, y0: 202.0, y1: 214.0 }
          : Nil
      )
    +> ( { color: colorB2, x0: 0.0, x1: 321.0, y0: 352.0, y1: 484.0 }
          : { color: colorB0, x0: 331.0, x1: 457.0, y0: 34.0, y1: 146.0 }
          : { color: colorB7, x0: 322.0, x1: 422.0, y0: 271.0, y1: 411.0 }
          : { color: colorB3, x0: 322.0, x1: 511.0, y0: 412.0, y1: 511.0 }
          : { color: colorB4, x0: 423.0, x1: 511.0, y0: 147.0, y1: 411.0 }
          : { color: colorB5, x0: 155.0, x1: 259.0, y0: 141.0, y1: 245.0 }
          : { color: colorB6, x0: 21.0, x1: 165.0, y0: 246.0, y1: 286.0 }
          : { color: colorB5, x0: 260.0, x1: 422.0, y0: 147.0, y1: 270.0 }
          : { color: colorB6, x0: 82.0, x1: 330.0, y0: 27.0, y1: 140.0 }
          : { color: colorB2, x0: 196.0, x1: 259.0, y0: 246.0, y1: 351.0 }
          : { color: colorB4, x0: 166.0, x1: 195.0, y0: 246.0, y1: 351.0 }
          : { color: colorB6, x0: 0.0, x1: 81.0, y0: 0.0, y1: 245.0 }
          : { color: colorB3, x0: 458.0, x1: 511.0, y0: 0.0, y1: 146.0 }
          : { color: colorB4, x0: 0.0, x1: 321.0, y0: 485.0, y1: 511.0 }
          : { color: colorB1, x0: 0.0, x1: 165.0, y0: 287.0, y1: 351.0 }
          : { color: colorB3, x0: 82.0, x1: 457.0, y0: 0.0, y1: 26.0 }
          : { color: colorB4, x0: 0.0, x1: 20.0, y0: 246.0, y1: 286.0 }
          : { color: colorB1, x0: 82.0, x1: 154.0, y0: 141.0, y1: 245.0 }
          : { color: colorB0, x0: 260.0, x1: 321.0, y0: 271.0, y1: 351.0 }
          : { color: colorB0, x0: 331.0, x1: 457.0, y0: 27.0, y1: 33.0 }
          : { color: colorB7, x0: 260.0, x1: 330.0, y0: 141.0, y1: 146.0 }
          : Nil
      )
    +> ( { color: colorB7, x0: 193.0, x1: 233.0, y0: 125.0, y1: 261.0 }
          : { color: colorB2, x0: 143.0, x1: 153.0, y0: 167.0, y1: 215.0 }
          : { color: colorB4, x0: 0.0, x1: 278.0, y0: 0.0, y1: 97.0 }
          : { color: colorB6, x0: 282.0, x1: 492.0, y0: 185.0, y1: 311.0 }
          : { color: colorB1, x0: 38.0, x1: 134.0, y0: 358.0, y1: 406.0 }
          : { color: colorB7, x0: 409.0, x1: 477.0, y0: 469.0, y1: 511.0 }
          : { color: colorB3, x0: 189.0, x1: 309.0, y0: 345.0, y1: 425.0 }
          : { color: colorB1, x0: 170.0, x1: 362.0, y0: 426.0, y1: 511.0 }
          : { color: colorB5, x0: 279.0, x1: 511.0, y0: 0.0, y1: 184.0 }
          : { color: colorB0, x0: 0.0, x1: 169.0, y0: 407.0, y1: 511.0 }
          : { color: colorB5, x0: 0.0, x1: 188.0, y0: 216.0, y1: 357.0 }
          : { color: colorB6, x0: 189.0, x1: 281.0, y0: 262.0, y1: 344.0 }
          : { color: colorB6, x0: 282.0, x1: 511.0, y0: 312.0, y1: 344.0 }
          : { color: colorB6, x0: 0.0, x1: 192.0, y0: 98.0, y1: 166.0 }
          : { color: colorB7, x0: 363.0, x1: 511.0, y0: 345.0, y1: 468.0 }
          : { color: colorB3, x0: 310.0, x1: 362.0, y0: 345.0, y1: 425.0 }
          : { color: colorB2, x0: 234.0, x1: 278.0, y0: 136.0, y1: 208.0 }
          : { color: colorB6, x0: 0.0, x1: 142.0, y0: 167.0, y1: 215.0 }
          : { color: colorB2, x0: 234.0, x1: 281.0, y0: 209.0, y1: 261.0 }
          : { color: colorB4, x0: 154.0, x1: 192.0, y0: 167.0, y1: 215.0 }
          : { color: colorB1, x0: 0.0, x1: 37.0, y0: 358.0, y1: 406.0 }
          : { color: colorB3, x0: 363.0, x1: 408.0, y0: 469.0, y1: 511.0 }
          : { color: colorB7, x0: 234.0, x1: 278.0, y0: 98.0, y1: 135.0 }
          : { color: colorB4, x0: 493.0, x1: 511.0, y0: 185.0, y1: 311.0 }
          : { color: colorB5, x0: 135.0, x1: 188.0, y0: 358.0, y1: 406.0 }
          : { color: colorB7, x0: 478.0, x1: 511.0, y0: 469.0, y1: 511.0 }
          : { color: colorB7, x0: 193.0, x1: 233.0, y0: 98.0, y1: 124.0 }
          : { color: colorB0, x0: 170.0, x1: 188.0, y0: 407.0, y1: 425.0 }
          : { color: colorB0, x0: 189.0, x1: 192.0, y0: 216.0, y1: 261.0 }
          : { color: colorB1, x0: 279.0, x1: 281.0, y0: 185.0, y1: 208.0 }
          : Nil
      )
    +> V.empty

squareSize = 512.0 :: Number

tilesForPiece :: V.Vec D16 (List TileBuilder2)
tilesForPiece = (map <<< map) (\i -> { color: i.color, x: i.x0 / squareSize, width: (i.x1 + 1.0 - i.x0) / squareSize, y: i.y0 / squareSize, height: (i.y1 + 1.0 - i.y0) / squareSize }) (V.take d16 tiles)
