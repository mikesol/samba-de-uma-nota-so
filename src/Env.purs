module SambaDeUmaNotaSo.Env where

import Prelude
import Color (rgb)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import Heterogeneous.Mapping (hmap)
import Prim.Row (class Lacks)
import Record as R
import SambaDeUmaNotaSo.Constants (windowLength)
import SambaDeUmaNotaSo.Types (AugmentedEnv, BaseEnv, FirstPartEnv, Windows, RGB)
import SambaDeUmaNotaSo.Util (argb, isRectangleTouched, rgbx, windowColors, windowToRect, xrgb, zipRecord)
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
      hmap
        (isRectangleTouched i.interactions)
        (windowToRect i.canvas.w i.canvas.h)

    mostRecentWindowInteraction =
      zipRecord
        (hmap (\x y -> if x then pure i.time else y) isWindowTouched)
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
  Lacks "windowOnScreen" r =>
  { time :: Number
  , canvas :: { w :: Number, h :: Number }
  , mostRecentWindowInteraction :: Windows (Maybe Number)
  | r
  } ->
  { time :: Number
  , canvas :: { w :: Number, h :: Number }
  , mostRecentWindowInteraction :: Windows (Maybe Number)
  , windowOnScreen :: Windows Painting
  | r
  }
withWindowOnScreen i@{ canvas, mostRecentWindowInteraction, time } =
  let
    rcts = windowToRect canvas.w canvas.h

    curriedFn0 = hmap (paintWindowOnScreen time) mostRecentWindowInteraction

    curriedFn1 = zipRecord curriedFn0 rcts

    windowOnScreen = zipRecord curriedFn1 windowColors
  in
    R.insert (Proxy :: _ "windowOnScreen") windowOnScreen i

{-
withWindowAndVideoOnScreen ::
  forall x r.
  Lacks "windowAndVideoOnScreen" r =>
  { | VideoPlayingInfo' x } ->
  Env { | AddWindowAndVideoOnScreen' r }
    ~> Env { | AddWindowAndVideoOnScreen' + WithWindowAndVideoOnScreen' + r }
withWindowAndVideoOnScreen { window, videoSpan } =
  withEnv \i@{ canvas
  , windowOnScreen
  , time
  } ->
    let
      windowAndVideoOnScreen w
        | w == window =
          let
            rct = windowToRect canvas.w canvas.h w
          in
            filled
              (fillColor (rgb 255 255 255))
              (rectangle rct.x rct.y rct.width rct.height)
              <> filled
                  (fillColor (rgba 0 0 0 (bindBetween 0.0 1.0 (calcSlope (videoSpan.start) 0.0 (videoSpan.start + videoSpan.duration) 1.0 time))))
                  (rectangle rct.x rct.y rct.width rct.height)
        | otherwise = windowOnScreen w
    in
      R.insert (Proxy :: _ "windowAndVideoOnScreen") windowAndVideoOnScreen i
-}
