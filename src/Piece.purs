module Crap where

{-
import Prelude
import Color (rgb, rgba)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree as Cf
import Control.Monad.Reader (Reader, ask, runReader)
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


withAugmentedEnv :: InteractionMap -> Reader (Record Env) (Record AugmentedEnv)
withAugmentedEnv prevInter =
  ask
    <#> \env@{ interactions, canvas: { w, h } } ->
        env
          `R.union`
            { freshTouches: getFreshTouches prevInter interactions
            , background:
                filled (fillColor (rgb 0 0 0))
                  (rectangle 0.0 0.0 w h)
            }

withFirstPartEnv :: Window' OnsetList -> Reader (Record AugmentedEnv) (Record FirstPartEnv)
withFirstPartEnv prevWindowInteractions =
  ask
    <#> \env@{ canvas, time, freshTouches } ->
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

withWindowOnScreen :: forall r. Reader (Record (AddWindowOnScreen' r)) (Record (AddWindowOnScreen' (WithWindowOnScreen' r)))
withWindowOnScreen = ask <#> addWindowOnScreen

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

type AddWindowAndVideoOnScreen' r
  = WithWindowOnScreen' + WithCanvas' + WithTime' + r

addWindowAndVideoOnScreen :: forall x r. Record (VideoPlayingInfo' x) -> Record (AddWindowAndVideoOnScreen' + r) -> Record (AddWindowAndVideoOnScreen' + WithWindowAndVideoOnScreen' + r)
addWindowAndVideoOnScreen { window, videoSpan } env@{ canvas
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
            <> filled
                (fillColor (rgba 0 0 0 (bindBetween 0.0 1.0 (calcSlope (videoSpan.start) 0.0 (videoSpan.start + videoSpan.duration) 1.0 time))))
                (rectangle rct.x rct.y rct.width rct.height)
      | otherwise = windowOnScreen w
  in
    { windowAndVideoOnScreen } `R.union` env

withWindowAndVideoOnScreen :: forall x r. Record (VideoPlayingInfo' x) -> Reader (Record (AddWindowAndVideoOnScreen' r)) (Record (AddWindowAndVideoOnScreen' (WithWindowAndVideoOnScreen' r)))
withWindowAndVideoOnScreen = (<#>) ask <<< addWindowAndVideoOnScreen

makeInfoForFirstPartEnv :: forall r. Record (WithInteractions' (WithWindowInteractions' r)) -> RPreFirstVideoInfo
makeInfoForFirstPartEnv { interactions, windowInteractions } =
  { prevInter: interactions
  , windowInteractions: memoize windowInteractions
  }

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

type OneNoteOutput
  = { audio :: AudioUnitD2
    , visual :: Map MeasurableText { width :: Number } -> Painting
    , accumulator :: OneNoteInfo
    }

dummyOutput =
  { audio: zero, visual: const mempty, accumulator: Terminus
  } ::
    OneNoteOutput

composeReadersFlipped :: forall a b c. Reader b c -> Reader a b -> Reader a c
composeReadersFlipped = map <<< runReader

composeReaders :: forall a b c. Reader a b -> Reader b c -> Reader a c
composeReaders = flip composeReadersFlipped

infixr 9 composeReadersFlipped as <|<

infixr 9 composeReaders as >|>

interpretFirstVideo :: FirstVideo' -> Reader (Record (WithWindowOnScreen' FirstPartEnv)) OneNoteOutput
interpretFirstVideo ac@{ window, videoSpan } =
  withWindowAndVideoOnScreen ac
    >|> ask
    <#> \env@{ canvas
      , background
      , windowAndVideoOnScreen
      } ->
        { audio: zero
        , visual: \_ -> background <> (fold (map windowAndVideoOnScreen windows))
        , accumulator:
            FirstVideo
              ({ window, videoSpan } `R.union` (makeInfoForFirstPartEnv env))
        }

interpretSecondVideo :: SecondVideo' -> Reader (Record (WithWindowOnScreen' FirstPartEnv)) OneNoteOutput
interpretSecondVideo ac@{ window, videoSpan } =
  withWindowAndVideoOnScreen ac
    >|> ask
    <#> \env@{ canvas
      , background
      , windowAndVideoOnScreen
      } ->
        { audio: zero
        , visual: \_ -> background <> (fold (map windowAndVideoOnScreen windows))
        , accumulator:
            SecondVideo
              ({ window, videoSpan } `R.union` (makeInfoForFirstPartEnv env))
        }

interpretThirdVideo :: ThirdVideo' -> Reader (Record (WithWindowOnScreen' FirstPartEnv)) OneNoteOutput
interpretThirdVideo ac@{ window, videoSpan } =
  withWindowAndVideoOnScreen ac
    >|> ask
    <#> \env@{ canvas
      , background
      , windowAndVideoOnScreen
      } ->
        { audio: zero
        , visual: \_ -> background <> (fold (map windowAndVideoOnScreen windows))
        , accumulator:
            SecondVideo
              ({ window, videoSpan } `R.union` (makeInfoForFirstPartEnv env))
        }

getTouchedWindow :: (Window -> Boolean) -> Maybe Window
getTouchedWindow f = tailRec go windows
  where
  go Nil = Done Nothing

  go (a : b) = if f a then Done (Just a) else Loop b

interpret :: OneNoteInfo -> Reader (Record Env) OneNoteOutput
interpret (PreFirstVideo ac) =
  if M.size ac.prevInter >= 5 then
    ask
      >>= \{ time } ->
          (interpret <<< AwaitingFirstVideo <<< R.union ac)
            { window: (intToWindow (floor ((time * 7.0) % 7.0)))
            }
  else
    withAugmentedEnv ac.prevInter
      >|> withFirstPartEnv ac.windowInteractions
      >|> withWindowOnScreen
      >|> ask
      <#> \env@{ background
        , windowOnScreen
        } ->
          { audio: zero
          , visual: \_ -> background <> (fold (map windowOnScreen windows))
          , accumulator: PreFirstVideo (makeInfoForFirstPartEnv env)
          }

interpret (AwaitingFirstVideo ac@{ window }) =
  withAugmentedEnv ac.prevInter
    >|> withFirstPartEnv ac.windowInteractions
    >|> withWindowOnScreen
    >|> do
        { isWindowTouched, time } <- ask
        if isWindowTouched ac.window then
          (interpretFirstVideo <<< R.union ac)
            { videoSpan:
                { start: time
                , duration: (firstVocalDuration time)
                }
            }
        else
          ask
            <#> \env@{ background
              , windowOnScreen
              } ->
                { audio: zero
                , visual: \_ -> background <> (fold (map windowOnScreen windows))
                , accumulator:
                    AwaitingFirstVideo ({ window } `R.union` (makeInfoForFirstPartEnv env))
                }

interpret (FirstVideo ac@{ prevInter, windowInteractions }) =
  ask
    >>= \{ time } ->
        if ac.videoSpan.start + ac.videoSpan.duration > time then
          interpret
            ( PreSecondVideo
                { prevInter
                , windowInteractions
                , nSincePrevInter: M.size ac.prevInter
                }
            )
        else
          withAugmentedEnv ac.prevInter
            >|> withFirstPartEnv ac.windowInteractions
            >|> withWindowOnScreen
            >|> interpretFirstVideo ac

interpret (PreSecondVideo { nSincePrevInter, prevInter, windowInteractions }) =
  if M.size prevInter + 3 >= nSincePrevInter then
    ask
      >>= \{ time } ->
          interpret
            ( AwaitingSecondVideo
                { window: (intToWindow (floor ((time * 7.0) % 7.0)))
                , prevInter
                , windowInteractions
                }
            )
  else
    withAugmentedEnv prevInter
      >|> withFirstPartEnv windowInteractions
      >|> withWindowOnScreen
      >|> ask
      <#> \env@{ background
        , windowOnScreen
        } ->
          { audio: zero
          , visual: \_ -> background <> (fold (map windowOnScreen windows))
          , accumulator:
              PreSecondVideo
                ( { nSincePrevInter }
                    `R.union`
                      (makeInfoForFirstPartEnv env)
                )
          }

interpret (AwaitingSecondVideo ac@{ window }) =
  withAugmentedEnv ac.prevInter
    >|> withFirstPartEnv ac.windowInteractions
    >|> withWindowOnScreen
    >|> do
        { isWindowTouched, time } <- ask
        if isWindowTouched ac.window then
          (interpretSecondVideo <<< R.union ac)
            { videoSpan:
                { start: time
                , duration: (firstVocalDuration time)
                }
            }
        else
          ask
            <#> \env@{ background
              , windowOnScreen
              } ->
                { audio: zero
                , visual: \_ -> background <> (fold (map windowOnScreen windows))
                , accumulator:
                    AwaitingSecondVideo ({ window } `R.union` (makeInfoForFirstPartEnv env))
                }

interpret (SecondVideo ac@{ prevInter, windowInteractions }) =
  ask
    >>= \{ time } ->
        if ac.videoSpan.start + ac.videoSpan.duration > time then
          interpret
            ( PreThirdVideo
                { prevInter
                , windowInteractions
                , nSincePrevInter: M.size ac.prevInter
                }
            )
        else
          withAugmentedEnv ac.prevInter
            >|> withFirstPartEnv ac.windowInteractions
            >|> withWindowOnScreen
            >|> interpretSecondVideo ac

interpret (PreThirdVideo { nSincePrevInter, prevInter, windowInteractions }) =
  withAugmentedEnv prevInter
    >|> withFirstPartEnv windowInteractions
    >|> do
        { isWindowTouched } <- ask
        case unit of
          _
            | M.size prevInter + 2 >= nSincePrevInter
            , Just window <- getTouchedWindow isWindowTouched ->
              withWindowOnScreen
                >|> do
                    { time } <- ask
                    interpretThirdVideo
                      { window
                      , prevInter
                      , windowInteractions
                      , videoSpan:
                          { start: time
                          , duration: (firstVocalDuration time)
                          }
                      }
          otherwise ->
            withWindowOnScreen
              >|> ask
              <#> \env@{ background
                , windowOnScreen
                } ->
                  { audio: zero
                  , visual: \_ -> background <> (fold (map windowOnScreen windows))
                  , accumulator:
                      PreThirdVideo
                        ( { nSincePrevInter }
                            `R.union`
                              (makeInfoForFirstPartEnv env)
                        )
                  }

interpret _ = pure dummyOutput

type PreFirstVideo'
  = Record
      ( WithPrevInter'
          + WithMemoizedWindowInteractions'
          + ()
      )

type AwaitingFirstVideo'
  = Record
      ( WithWindow'
          + WithPrevInter'
          + WithMemoizedWindowInteractions'
          + ()
      )

type FirstVideo'
  = Record
      ( WithVideoSpan'
          + WithWindow'
          + WithPrevInter'
          + WithMemoizedWindowInteractions'
          + ()
      )

type PreSecondVideo'
  = Record
      ( WithNSincePrevInter'
          + WithPrevInter'
          + WithMemoizedWindowInteractions'
          + ()
      )

type AwaitingSecondVideo'
  = Record
      ( WithWindow'
          + WithPrevInter'
          + WithMemoizedWindowInteractions'
          + ()
      )

type SecondVideo'
  = Record
      ( WithVideoSpan'
          + WithWindow'
          + WithPrevInter'
          + WithMemoizedWindowInteractions'
          + ()
      )

type PreThirdVideo'
  = Record
      ( WithNSincePrevInter'
          + WithPrevInter'
          + WithMemoizedWindowInteractions'
          + ()
      )

type ThirdVideo'
  = Record
      ( WithVideoSpan'
          + WithWindow'
          + WithPrevInter'
          + WithMemoizedWindowInteractions'
          + ()
      )

data OneNoteInfo
  = PreFirstVideo PreFirstVideo'
  | AwaitingFirstVideo AwaitingFirstVideo'
  | FirstVideo FirstVideo'
  | PreSecondVideo PreSecondVideo'
  | AwaitingSecondVideo AwaitingSecondVideo'
  | SecondVideo SecondVideo'
  | PreThirdVideo PreThirdVideo'
  | ThirdVideo ThirdVideo'
  | Terminus

scene :: Interactions -> OneNoteInfo -> CanvasInfo -> Number -> Behavior (AV D2 OneNoteInfo)
scene inter acc (CanvasInfo { w, h, boundingClientRect }) time = go <$> interactionLog inter
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
      runReader (interpret acc)
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

main :: Klank' OneNoteInfo
main =
  klank
    { run = runInBrowser_ (scene <$> getInteractivity)
    , engineInfo = \res rej -> res sambaEngineInfo
    , accumulator =
      \res _ ->
        res
          ( PreFirstVideo
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
-}