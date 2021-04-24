module SambaDeUmaNotaSo.Env where

import Prelude
import Color (rgb, rgba)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Lt, class Nat, D7)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import Prim.Row (class Lacks)
import Record as R
import SambaDeUmaNotaSo.Constants (windowLength)
import SambaDeUmaNotaSo.Types (AugmentedEnv, BaseEnv, FirstPartEnv, Windows, RGB)
import SambaDeUmaNotaSo.Util (argb, bindBetween, calcSlope, isRectangleTouched, rgbx, windowColors, windowToRect, xrgb)
import Type.Proxy (Proxy(..))

withAugmentedEnv :: BaseEnv -> AugmentedEnv
withAugmentedEnv i =
  R.union i
    { background:
        filled (fillColor (rgb 0 0 0))
          (rectangle 0.0 0.0 i.canvas.w i.canvas.h)
    }

withFirstPartEnv :: Windows (Maybe Number) -> AugmentedEnv -> FirstPartEnv
withFirstPartEnv prevMostRecentWindowInteraction i =
  let
    isWindowTouched =
      map
        (isRectangleTouched i.interactions)
        (windowToRect i.canvas.w i.canvas.h)

    mostRecentWindowInteraction =
      V.zipWithE
        (\x y -> if x then pure i.time else y)
        isWindowTouched
        prevMostRecentWindowInteraction
  in
    R.union i { isWindowTouched, mostRecentWindowInteraction }

paintWindowOnScreen :: Number -> Maybe Number -> Rectangle -> RGB -> Painting
paintWindowOnScreen time mostRecentWindowInteraction windowDims windowColor =
  filled
    ( fillColor
        ( case mostRecentWindowInteraction of
            Nothing -> rgb 0 0 0
            Just onset
              | time - onset < windowLength ->
                rgbx
                  ( argb
                      onset
                      windowColor
                      (onset + windowLength)
                      (xrgb 0 0 0)
                      time
                  )
              | otherwise -> rgb 0 0 0
        )
    )
    (rectangle windowDims.x windowDims.y windowDims.width windowDims.height)

withWindowOnScreen ::
  forall r.
  Lacks "windowDims" r =>
  Lacks "windowsOnScreen" r =>
  { time :: Number
  , canvas :: { w :: Number, h :: Number }
  , mostRecentWindowInteraction :: Windows (Maybe Number)
  | r
  } ->
  { time :: Number
  , canvas :: { w :: Number, h :: Number }
  , mostRecentWindowInteraction :: Windows (Maybe Number)
  , windowDims :: Windows Rectangle
  , windowsOnScreen :: Windows Painting
  | r
  }
withWindowOnScreen i@{ canvas, mostRecentWindowInteraction, time } =
  let
    rcts = windowToRect canvas.w canvas.h

    curriedFn0 = map (paintWindowOnScreen time) mostRecentWindowInteraction

    curriedFn1 = V.zipWithE ($) curriedFn0 rcts

    windowsOnScreen = V.zipWithE ($) curriedFn1 windowColors
  in
    R.insert (Proxy :: _ "windowDims") rcts
      (R.insert (Proxy :: _ "windowsOnScreen") windowsOnScreen i)

withWindowAndVideoOnScreen ::
  forall nat x r.
  Nat nat =>
  Lt nat D7 =>
  Lacks "windowsAndVideoOnScreen" r =>
  { window :: nat
  , videoSpan :: { start :: Number, duration :: Number }
  | x
  } ->
  { time :: Number
  , windowsOnScreen :: Windows Painting
  , windowDims :: Windows Rectangle
  | r
  } ->
  { time :: Number
  , windowDims :: Windows Rectangle
  , windowsOnScreen :: Windows Painting
  , windowsAndVideoOnScreen :: Windows Painting
  | r
  }
withWindowAndVideoOnScreen { window, videoSpan } i@{ windowsOnScreen
, windowDims
, time
} =
  let
    rct = V.index windowDims window

    vid =
      filled
        (fillColor (rgb 255 255 255))
        (rectangle rct.x rct.y rct.width rct.height)
        <> filled
            ( fillColor
                ( rgba 0 0 0
                    ( bindBetween 0.0 1.0
                        ( calcSlope (videoSpan.start)
                            0.0
                            (videoSpan.start + videoSpan.duration)
                            1.0
                            time
                        )
                    )
                )
            )
            (rectangle rct.x rct.y rct.width rct.height)

    windowsAndVideoOnScreen = V.updateAt window vid windowsOnScreen
  in
    R.insert (Proxy :: _ "windowsAndVideoOnScreen") windowsAndVideoOnScreen i
