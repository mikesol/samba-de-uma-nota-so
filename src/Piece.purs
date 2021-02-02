module Klank.Dev where

import Prelude
import Color (rgb, rgba)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree as Cf
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as A
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (fold, foldl, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.Int (floor, toNumber)
import Data.Lens (_2, _Left, over, traversed)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, insert, lookup, update)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Profunctor (lcmap)
import Data.Set (Set, member, union)
import Data.Set as S
import Data.String (indexOf, Pattern(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (class Pos, D2, D8, d0, d1, d2, d3, d4, d5, d6, d7)
import Data.Vec (Vec, (+>), empty)
import Data.Vec as V
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioUnit, CanvasInfo(..), EngineInfo, defaultExporter, defaultParam, gain_, gain_', loopBuf_, makePeriodicWave, pannerMono_, periodicOsc_, playBufT_, playBufWithOffset_, playBuf_, runInBrowser_, speaker, speaker')
import FRP.Event (Event, makeEvent, subscribe)
import Foreign.Object as O
import Graphics.Canvas (Rectangle)
import Graphics.Drawing (Color, Font, Point)
import Graphics.Drawing.Font (bold, font, sansSerif)
import Graphics.Painting (FillStyle, Gradient(..), ImageSource(..), MeasurableText, Painting, circle, drawImageFull, fillColor, fillGradient, filled, rectangle, text, textMeasurableText)
import Klank.Dev.Util (makeBuffersKeepingCache, makeImagesKeepingCache)
import Math (cos, pi, pow, sin, (%))
import Record as R
import Type.Klank.Dev (Klank', defaultEngineInfo, klank)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Navigator (userAgent)
import Web.HTML.Window (navigator, toEventTarget)
import Web.TouchEvent (TouchEvent)
import Web.TouchEvent.Touch (identifier)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent (changedTouches)
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.TouchList as TL
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

----- engine
sambaEngineInfo =
  defaultEngineInfo
    { msBetweenSamples = 50
    , msBetweenPings = 45
    } ::
    EngineInfo

----- constants
bpm = 160.0 :: Number

beat = 60.0 / bpm :: Number

windowLength = beat * 4.0 :: Number

kr = toNumber sambaEngineInfo.msBetweenSamples / 1000.0 :: Number

start = 0.0 :: Number

ptTop0 = 0.25 :: Number

ptTop1 = 0.7 :: Number

ptLeft0 = 0.45 :: Number

ptLeft1 = 0.75 :: Number

ptBottom0 = 0.33 :: Number

ptRight0 = 0.25 :: Number

end = 1.0 :: Number

----- classes
class Memoizable f g | f -> g, g -> f where
  memoize :: Function f ~> g
  functionize :: g ~> Function f

----- instances
instance memoizableWindow :: Memoizable Window Window' where
  memoize f =
    Window'
      { w0: f W0
      , w1: f W1
      , w2: f W2
      , w3: f W3
      , w4: f W4
      , w5: f W5
      , w6: f W6
      }
  functionize (Window' { w0 }) W0 = w0
  functionize (Window' { w1 }) W1 = w1
  functionize (Window' { w2 }) W2 = w2
  functionize (Window' { w3 }) W3 = w3
  functionize (Window' { w4 }) W4 = w4
  functionize (Window' { w5 }) W5 = w5
  functionize (Window' { w6 }) W6 = w6

----- type
data Window
  = W0
  | W1
  | W2
  | W3
  | W4
  | W5
  | W6

derive instance eqWindow :: Eq Window

newtype Window' a
  = Window'
  { w0 :: a
  , w1 :: a
  , w2 :: a
  , w3 :: a
  , w4 :: a
  , w5 :: a
  , w6 :: a
  }

type AudioUnitD2
  = AudioUnit D2

type SambaAcc'
  = { audio :: AudioUnitD2
    , visual :: Map MeasurableText { width :: Number } -> Painting
    , accumulator :: SambaAcc
    }

newtype SambaAcc
  = SambaAcc
  (Function (Record Env) SambaAcc')

derive instance newtypeSambaAcc :: Newtype SambaAcc _

type WithTime' r
  = ( time :: Number | r )

type WithInteractions' r
  = ( interactions :: InteractionMap | r )

type WithCanvas' r
  = ( canvas :: { w :: Number, h :: Number } | r )

type Env' r
  = (WithTime' + WithCanvas' + WithInteractions' + r)

type Env
  = Env' ()

type WithFreshTouches' r
  = ( freshTouches :: InteractionMap | r )

type WithBackground' r
  = ( background :: Painting | r )

type AugmentedEnv' r
  = (WithFreshTouches' + WithBackground' + r)

type AugmentedEnv
  = AugmentedEnv' Env

type WithWindowInteractions' r
  = ( windowInteractions :: Window -> List { onset :: Number } | r )

type WithIsWindowTouched' r
  = ( isWindowTouched :: Window -> Boolean | r )

type FirstPartEnv' r
  = (WithWindowInteractions' + WithIsWindowTouched' + r)

type FirstPartEnv
  = FirstPartEnv' AugmentedEnv

type WithWindowOnScreen' r
  = ( windowOnScreen :: Window -> Painting | r )

type WithWindowAndVideoOnScreen' r
  = ( windowAndVideoOnScreen :: Window -> Painting | r )

type WithWindow' r
  = ( window :: Window | r )

type WithVideoSpan' r
  = ( videoSpan :: Tuple Number Number | r )

type VideoPlayingInfo' r
  = (WithWindow' + WithVideoSpan' + r)

type RGB
  = { r :: Int, g :: Int, b :: Int }

type OnsetList
  = List { onset :: Number }

type WithPrevInter' r
  = ( prevInter :: InteractionMap | r )

type WithMemoizedWindowInteractions' r
  = ( windowInteractions :: Window' OnsetList | r )

type InfoForFirstPartEnv r
  = WithMemoizedWindowInteractions' (WithPrevInter' r)

type RPreFirstVideoInfo
  = Record (InfoForFirstPartEnv ())

type RAwaitingFirstVideoInfo
  = Record (InfoForFirstPartEnv (WithWindow' ()))

type RFirstVideoInfo
  = Record (InfoForFirstPartEnv (VideoPlayingInfo' ()))

----- util
intToWindow :: Int -> Window
intToWindow = case _ of
  0 -> W0
  1 -> W1
  2 -> W2
  3 -> W3
  4 -> W4
  5 -> W5
  6 -> W6
  _ -> W0

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

bindBetween :: Number -> Number -> Number -> Number
bindBetween mn mx n = max mn (min mx n)

inRect :: Point -> Number -> Number -> Number -> Number -> Boolean
inRect pt x y w h = pt.x >= x && pt.x <= x + w && pt.y >= y && pt.y <= y + h

xrgb :: Int -> Int -> Int -> RGB
xrgb r g b = { r, g, b }

rgbx :: RGB -> Color
rgbx { r, g, b } = rgb r g b

argb :: Number -> RGB -> Number -> RGB -> Number -> RGB
argb t0 c0 t1 c1 t =
  { r: cs c0.r c1.r
  , g: cs c0.g c1.g
  , b: cs c0.b c1.b
  }
  where
  cs x y = floor (bindBetween 0.0 255.0 (calcSlope t0 (toNumber x) t1 (toNumber y) t))

windowColors :: Window -> RGB
windowColors = case _ of
  W0 -> xrgb 194 233 251
  W1 -> xrgb 250 208 196
  W2 -> xrgb 255 236 210
  W3 -> xrgb 254 207 239
  W4 -> xrgb 226 235 240
  W5 -> xrgb 102 126 234
  W6 -> xrgb 253 252 251

windowCoords :: Window -> Rectangle
windowCoords = case _ of
  W0 ->
    { x: start
    , y: start
    , width: ptTop0
    , height: ptLeft0
    }
  W1 ->
    { x: ptTop0
    , y: start
    , width: end - ptTop0
    , height: ptRight0
    }
  W2 ->
    { x: ptTop0
    , y: ptRight0
    , width: ptTop1 - ptTop0
    , height: ptLeft0 - ptRight0
    }
  W3 ->
    { x: ptTop1
    , y: ptRight0
    , width: end - ptTop1
    , height: ptLeft1 - ptRight0
    }
  W4 ->
    { x: start
    , y: ptLeft0
    , width: ptTop1
    , height: ptLeft1 - ptLeft0
    }
  W5 ->
    { x: start
    , y: ptLeft1
    , width: ptBottom0
    , height: end - ptLeft1
    }
  W6 ->
    { x: ptBottom0
    , y: ptLeft1
    , width: end - ptBottom0
    , height: end - ptLeft1
    }

augmentedEnv :: InteractionMap -> Function (Record Env) (Record AugmentedEnv)
augmentedEnv prevInter env@{ interactions, canvas: { w, h } } =
  env
    `R.union`
      { freshTouches: getFreshTouches prevInter interactions
      , background:
          filled (fillColor (rgb 0 0 0))
            (rectangle 0.0 0.0 w h)
      }

firstPartEnv :: Window' OnsetList -> Function (Record AugmentedEnv) (Record FirstPartEnv)
firstPartEnv prevWindowInteractions env@{ canvas, time, freshTouches } =
  let
    isWindowTouched =
      isRectangleTouched
        (M.values freshTouches)
        <<< windowToRect canvas.w canvas.h

    windowInteractions w = let wl = (functionize prevWindowInteractions) w in if isWindowTouched w then { onset: time } : wl else wl
  in
    env `R.union` { isWindowTouched, windowInteractions }

type AddWindowOnScreen' r
  = WithCanvas' + WithWindowInteractions' + WithTime' + r

withWindowOnScreen :: forall r a. Function (Record (AddWindowOnScreen' (WithWindowOnScreen' r))) a -> Function (Record (AddWindowOnScreen' r)) a
withWindowOnScreen = (>>>) addWindowOnScreen

addWindowOnScreen :: forall r. Record (AddWindowOnScreen' r) -> Record (AddWindowOnScreen' + WithWindowOnScreen' + r)
addWindowOnScreen env@{ canvas
, windowInteractions
, time
} =
  let
    windowOnScreen w =
      let
        rct = windowToRect canvas.w canvas.h w
      in
        filled
          ( fillColor
              ( case windowInteractions w of
                  Nil -> rgb 0 0 0
                  { onset } : b
                    | time - onset < windowLength ->
                      rgbx
                        ( argb
                            onset
                            (windowColors w)
                            (onset + windowLength)
                            (xrgb 0 0 0)
                            time
                        )
                    | otherwise -> rgb 0 0 0
              )
          )
          (rectangle rct.x rct.y rct.width rct.height)
  in
    { windowOnScreen } `R.union` env

type AddWindowPlusVideoOnScreen' r
  = WithWindowOnScreen' + WithCanvas' + WithTime' + r

addWindowPlusVideoOnScreen :: forall x r. Record (VideoPlayingInfo' x) -> Record (AddWindowPlusVideoOnScreen' + r) -> Record (AddWindowPlusVideoOnScreen' + WithWindowAndVideoOnScreen' + r)
addWindowPlusVideoOnScreen { window, videoSpan } env@{ canvas
, windowOnScreen
, time
} =
  let
    windowAndVideoOnScreen w
      | w == window =
        let
          rct = windowToRect canvas.w canvas.h w
        in
          filled
            (fillColor (rgb 255 255 255))
            (rectangle rct.x rct.y rct.width rct.height)
      | otherwise = windowOnScreen w
  in
    { windowAndVideoOnScreen } `R.union` env

withWindowAndVideoOnScreen :: forall x r a. Record (VideoPlayingInfo' x) -> Function (Record (AddWindowPlusVideoOnScreen' (WithWindowAndVideoOnScreen' r))) a -> Function (Record (AddWindowPlusVideoOnScreen' r)) a
withWindowAndVideoOnScreen = (>>>) <<< addWindowPlusVideoOnScreen

makeInfoForFirstPartEnv :: forall r. Record (WithInteractions' (WithWindowInteractions' r)) -> RPreFirstVideoInfo
makeInfoForFirstPartEnv { interactions, windowInteractions } =
  { prevInter: interactions
  , windowInteractions: memoize windowInteractions
  }

executeInFirstPartEnv :: forall r. Record (InfoForFirstPartEnv r) -> Function (Record FirstPartEnv) SambaAcc' -> SambaAcc
executeInFirstPartEnv { prevInter, windowInteractions } fpFunction =
  SambaAcc
    $ augmentedEnv prevInter
    >>> firstPartEnv windowInteractions
    >>> fpFunction

isRectangleTouched :: List Interaction -> Rectangle -> Boolean
isRectangleTouched l r = go l
  where
  go Nil = false

  go ({ pt: Left pt } : b) = inRect pt r.x r.y r.width r.height || go b

  go ({ pt: Right _ } : b) = go b

getFreshTouches :: InteractionMap -> InteractionMap -> InteractionMap
getFreshTouches = M.filterKeys <<< map not <<< flip M.member

scaleRect :: Number -> Number -> Rectangle -> Rectangle
scaleRect w h r = { x: r.x * w, y: r.y * h, width: r.width * w, height: r.height * h }

windowToRect :: Number -> Number -> Window -> Rectangle
windowToRect w h = scaleRect w h <<< windowCoords

windows = W0 : W1 : W2 : W3 : W4 : W5 : W6 : Nil :: List Window

preFirstVideo' :: forall r. Record (WithPrevInter' r) -> Function (Record FirstPartEnv) SambaAcc'
preFirstVideo' { prevInter } e@{ time } =
  if M.size prevInter >= 5 then
    awaitingFirstVideo'
      { window: (intToWindow (floor ((time * 7.0) % 7.0))) }
      e
  else
    withWindowOnScreen
      ( \env@{ canvas
        , background
        , interactions
        , windowOnScreen
        , windowInteractions
        } ->
          { audio: zero
          , visual: \_ -> background <> (fold (map windowOnScreen windows))
          , accumulator:
              preFirstVideo (makeInfoForFirstPartEnv env)
          }
      )
      e

preFirstVideo :: RPreFirstVideoInfo -> SambaAcc
preFirstVideo info =
  executeInFirstPartEnv
    info
    (preFirstVideo' info)

awaitingFirstVideo' :: forall r. Record (WithWindow' + r) -> Function (Record FirstPartEnv) SambaAcc'
awaitingFirstVideo' ipt@{ window } e@{ isWindowTouched, time } =
  if isWindowTouched window then
    firstVideo'
      { window, videoSpan: Tuple time time }
      e
  else
    withWindowOnScreen
      ( \env@{ canvas
        , background
        , interactions
        , windowOnScreen
        , windowInteractions
        } ->
          { audio: zero
          , visual: \_ -> background <> (fold (map windowOnScreen windows))
          , accumulator:
              awaitingFirstVideo ({ window } `R.union` (makeInfoForFirstPartEnv env))
          }
      )
      e

awaitingFirstVideo :: RAwaitingFirstVideoInfo -> SambaAcc
awaitingFirstVideo info =
  executeInFirstPartEnv
    info
    (awaitingFirstVideo' info)

firstVideo' :: forall r. Record (VideoPlayingInfo' r) -> Function (Record FirstPartEnv) SambaAcc'
firstVideo' x@{ window, videoSpan } =
  addWindowOnScreen
    >>> withWindowAndVideoOnScreen x
        ( \env@{ canvas
          , background
          , interactions
          , windowAndVideoOnScreen
          } ->
            { audio: zero
            , visual: \_ -> background <> (fold (map windowAndVideoOnScreen windows))
            , accumulator:
                firstVideo
                  ({ window, videoSpan } `R.union` (makeInfoForFirstPartEnv env))
            }
        )

firstVideo :: RFirstVideoInfo -> SambaAcc
firstVideo info =
  executeInFirstPartEnv
    info
    (firstVideo' info)

scene :: Interactions -> SambaAcc -> CanvasInfo -> Number -> Behavior (AV D2 SambaAcc)
scene inter (SambaAcc acc) (CanvasInfo { w, h, boundingClientRect }) time = go <$> interactionLog inter
  where
  go { interactions } =
    AV
      { audio: Just audio
      , visual:
          Just
            { painting: \{ words } -> visual words
            , words: Nil
            }
      , accumulator
      }
    where
    { audio, visual, accumulator } =
      acc
        { interactions:
            over ((prop (SProxy :: SProxy "pt")) <<< _Left)
              ( \{ x, y } ->
                  { x: x - boundingClientRect.x, y: y - boundingClientRect.y
                  }
              )
              <$> interactions
        , time
        , canvas: { w, h }
        }

main :: Klank' SambaAcc
main =
  klank
    { run = runInBrowser_ (scene <$> getInteractivity)
    , engineInfo = \res rej -> res sambaEngineInfo
    , accumulator =
      \res _ ->
        res
          ( preFirstVideo
              { prevInter: M.empty
              , windowInteractions: memoize (const Nil)
              }
          )
    , exporter = defaultExporter
    , webcamCache = \_ _ -> identity
    , periodicWaves =
      \ctx _ res rej -> res $ O.fromFoldable []
    , buffers =
      makeBuffersKeepingCache 20 []
    , images = makeImagesKeepingCache 20 []
    }

newtype Interactions
  = Interactions
  { interactions :: Ref.Ref InteractionMap
  , dispose :: Effect Unit
  }

type InteractionMap
  = Map Int Interaction

type Interaction
  = { pt :: Either Point Number -- either it has a place or has ended
    , onset :: Number
    }

purge :: Number -> InteractionMap -> InteractionMap
purge currentTime = M.filter \{ onset } -> onset + (20.0 * 1000.0) > currentTime

handleTE :: MAction -> Ref.Ref Int -> Ref.Ref (Map Int Int) -> Ref.Ref InteractionMap -> TouchEvent -> Effect Unit
handleTE mAction indxr rlsr ilk te = do
  tn <- map (unwrap <<< unInstant) now
  ctr' <- Ref.read indxr
  mppy' <- Ref.read rlsr
  il' <- Ref.read ilk
  let
    { ctr, mppy, il } =
      foldl
        ( \{ ctr, mppy, il } i ->
            let
              t' = TL.item i ts
            in
              case t' of
                Nothing -> { ctr, mppy, il }
                Just t -> case mAction of
                  MDown ->
                    let
                      newC = ctr + 1
                    in
                      { ctr: newC
                      , mppy: insert (identifier t) newC mppy
                      , il: insert newC ({ onset: tn, pt: Left { x: toNumber $ T.clientX t, y: toNumber $ T.clientY t } }) il
                      }
                  _ -> case lookup (identifier t) mppy of
                    Nothing -> { ctr, mppy, il }
                    Just newC ->
                      { ctr
                      , mppy
                      , il:
                          update
                            ( \v ->
                                Just
                                  v
                                    { pt =
                                      case mAction of
                                        MUp -> Right tn
                                        _ -> Left { x: toNumber $ T.clientX t, y: toNumber $ T.clientY t }
                                    }
                            )
                            newC
                            il
                      }
        )
        { ctr: ctr', mppy: mppy', il: il' }
        (L.range 0 (l - 1))
  Ref.write ctr indxr
  Ref.write mppy rlsr
  Ref.write il ilk
  where
  ts = changedTouches te

  l = TL.length ts

data MAction
  = MUp
  | MMove
  | MDown

data TState
  = TFree
  | TUsed

mapToTState :: Map Int TState -> Int -> TState
mapToTState m i = fromMaybe TFree $ lookup i m

ptize :: forall r. MouseEvent -> Either Point r
ptize me = Left { x: toNumber $ ME.clientX me, y: toNumber $ ME.clientY me }

handleME :: Int -> MAction -> Ref.Ref InteractionMap -> MouseEvent -> Effect Unit
handleME i mAction ref me = do
  tn <- map (unwrap <<< unInstant) now
  void
    $ Ref.modify
        ( case mAction of
            MDown ->
              insert i
                { onset: tn
                , pt: ptize me
                }
            MMove ->
              update
                ( \e ->
                    Just
                      $ e { pt = ptize me }
                )
                i
            MUp -> (purge tn) <<< (update (\e -> Just $ e { pt = Right tn }) i)
        )
        ref

makeTouchListener :: MAction -> Ref.Ref Int -> Ref.Ref (Map Int Int) -> Ref.Ref InteractionMap -> Effect EventListener
makeTouchListener mAction idxr rlsr interactions =
  eventListener \e -> do
    TE.fromEvent e
      # traverse_ \te -> do
          handleTE mAction idxr rlsr interactions te

makeMouseListener :: MAction -> Ref.Ref Int -> Ref.Ref InteractionMap -> Effect EventListener
makeMouseListener mAction ctr interactions =
  eventListener \e -> do
    ME.fromEvent e
      # traverse_ \me -> do
          nt <- case mAction of
            MUp -> Ref.modify (_ + 1) ctr
            _ -> Ref.read ctr
          handleME nt mAction interactions me

getInteractivity :: Effect Interactions
getInteractivity = do
  w <- window
  nav <- navigator w
  ua <- userAgent nav
  let
    mobile = isJust (indexOf (Pattern "iPhone") ua) || isJust (indexOf (Pattern "iPad") ua) || isJust (indexOf (Pattern "Android") ua)
  ctr <- Ref.new 0
  referencePosition <- Ref.new Nothing
  interactions <- Ref.new M.empty
  interactionIds <- Ref.new M.empty
  target <- toEventTarget <$> window
  touchStartListener <- makeTouchListener MDown ctr interactionIds interactions
  touchMoveListener <- makeTouchListener MMove ctr interactionIds interactions
  touchEndListener <- makeTouchListener MUp ctr interactionIds interactions
  mouseDownListener <- makeMouseListener MDown ctr interactions
  mouseMoveListener <- makeMouseListener MMove ctr interactions
  mouseUpListener <- makeMouseListener MUp ctr interactions
  if mobile then do
    addEventListener (wrap "touchstart") touchStartListener false target
    addEventListener (wrap "touchmove") touchMoveListener false target
    addEventListener (wrap "touchend") touchEndListener false target
  else do
    addEventListener (wrap "mousedown") mouseDownListener false target
    addEventListener (wrap "mousemove") mouseMoveListener false target
    addEventListener (wrap "mouseup") mouseUpListener false target
  let
    dispose =
      if mobile then do
        removeEventListener (wrap "touchstart") touchStartListener false target
        removeEventListener (wrap "touchmove") touchMoveListener false target
        removeEventListener (wrap "touchend") touchEndListener false target
      else do
        removeEventListener (wrap "mousedown") mouseDownListener false target
        removeEventListener (wrap "mousemove") mouseMoveListener false target
        removeEventListener (wrap "mouseup") mouseUpListener false target
  pure (Interactions { interactions, dispose })

withInteractions ::
  forall a.
  Interactions ->
  Event a ->
  Event { value :: a, interactions :: InteractionMap }
withInteractions (Interactions { interactions }) e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          interactionsValue <- Ref.read interactions
          k { value, interactions: interactionsValue }

interactionLog :: Interactions -> Behavior { interactions :: InteractionMap }
interactionLog m = behavior \e -> map (\{ value, interactions } -> value { interactions }) (withInteractions m e)
