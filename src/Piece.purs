module SambaDeUmaNotaSo.Piece where

import Prelude
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d3, d5)
import Data.Vec as V
import SambaDeUmaNotaSo.Constants (fourMeasures)
import SambaDeUmaNotaSo.Drawing (blackBackground)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideo, isVideoWindowTouched)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (awaitingFirstVideoCreate)
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (awaitingSecondVideoCreate)
import SambaDeUmaNotaSo.Loops.FirstVideo (firstVideoCreate)
import SambaDeUmaNotaSo.Loops.FourthVideo (fourthVideoCreate)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (preFirstVideoCreate)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (preSecondVideoCreate)
import SambaDeUmaNotaSo.Loops.PreThirdVideo (preThirdVideoCreate)
import SambaDeUmaNotaSo.Loops.SecondVideo (secondVideoCreate)
import SambaDeUmaNotaSo.Loops.ThirdVideo (thirdVideoCreate)
import SambaDeUmaNotaSo.Transitions.AwaitingFirstVideo (doAwaitingFirstVideo)
import SambaDeUmaNotaSo.Transitions.AwaitingSecondVideo (doAwaitingSecondVideo)
import SambaDeUmaNotaSo.Transitions.FirstVideo (doFirstVideo)
import SambaDeUmaNotaSo.Transitions.FourthVideo (doFourthVideo)
import SambaDeUmaNotaSo.Transitions.PreFirstVideo (doPreFirstVideo)
import SambaDeUmaNotaSo.Transitions.PreSecondVideo (doPreSecondVideo)
import SambaDeUmaNotaSo.Transitions.PreThirdVideo (doPreThirdVideo)
import SambaDeUmaNotaSo.Transitions.SecondVideo (doSecondVideo)
import SambaDeUmaNotaSo.Transitions.ThirdVideo (doThirdVideo, moveVideo)
import SambaDeUmaNotaSo.Util (beatModSeven)
import Type.Data.Peano as N
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (env, modifyRes, start, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame0, InitialFrameT)
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Example.KitchenSink.TLP.LoopSig (SambaSceneI, SceneSig, SambaRes)
import WAGS.Graph.Decorators (This(..))
import WAGS.Graph.Optionals (speaker)
import WAGS.Interpret (class AudioInterpret)
import WAGS.MoveNode (moveNode)

data StartAt
  = PreFirstVideo
  | AwaitingFirstVideo
  | FirstVideo
  | PreSecondVideo
  | AwaitingSecondVideo
  | SecondVideo
  | PreThirdVideo
  | ThirdVideo
  | FourthVideo

startAt = PreThirdVideo :: StartAt

startWithBlackBackground ::
  forall audio engine m.
  Monad m =>
  AudioInterpret audio engine =>
  InitialFrameT SambaSceneI audio engine m SambaRes Unit
startWithBlackBackground = WAGS.do
  start
  e <- env
  ivoid
    $ modifyRes
    $ const
        { painting: blackBackground e.world.canvas.width e.world.canvas.height
        }

-- | When working on the piece, we want to be able to start from any section.
-- | This splits up the start of the piece on a section-by-section basis
-- | as well as initial states for all sections.
piece :: SceneSig Frame0
piece = case startAt of
  PreFirstVideo ->
    WAGS.do
      startWithBlackBackground
      ivoid $ create preFirstVideoCreate
      cursorGain <- cursor (speaker This)
      moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
        $> { nTouchesSoFar: 0
          , mostRecentWindowInteraction: V.fill (const Nothing)
          , cursorGain
          }
      @|> doPreFirstVideo
  AwaitingFirstVideo ->
    WAGS.do
      startWithBlackBackground
      ivoid $ create awaitingFirstVideoCreate
      cursorGain <- cursor (speaker This)
      moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
        $> { interpretVideo: interpretVideo d3
          , isVideoWindowTouched: isVideoWindowTouched d3
          , mostRecentWindowInteraction: V.fill (const Nothing)
          , cursorGain
          }
      @|> doAwaitingFirstVideo
  FirstVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        ivoid $ create firstVideoCreate
        cursorGain <- cursor (speaker This)
        moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
          $> { interpretVideo: interpretVideo d3 videoSpan
            , mostRecentWindowInteraction: V.fill (const Nothing)
            , cursorGain
            , videoSpan
            }
        @|> doFirstVideo
  PreSecondVideo ->
    WAGS.do
      startWithBlackBackground
      ivoid $ create preSecondVideoCreate
      cursorGain <- cursor (speaker This)
      moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
        $> { nTouchesSoFar: 0
          , mostRecentWindowInteraction: V.fill (const Nothing)
          , cursorGain
          }
      @|> doPreSecondVideo
  AwaitingSecondVideo ->
    WAGS.do
      startWithBlackBackground
      ivoid $ create awaitingSecondVideoCreate
      cursorGain <- cursor (speaker This)
      moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
        $> { interpretVideo: interpretVideo d5
          , isVideoWindowTouched: isVideoWindowTouched d5
          , mostRecentWindowInteraction: V.fill (const Nothing)
          , cursorGain
          }
      @|> doAwaitingSecondVideo
  SecondVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        ivoid $ create secondVideoCreate
        cursorGain <- cursor (speaker This)
        moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
          $> { interpretVideo: interpretVideo d5 videoSpan
            , mostRecentWindowInteraction: V.fill (const Nothing)
            , cursorGain
            , videoSpan
            }
        @|> doSecondVideo
  PreThirdVideo ->
    WAGS.do
      startWithBlackBackground
      ivoid $ create preThirdVideoCreate
      cursorGain <- cursor (speaker This)
      moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
        $> { mostRecentWindowInteraction: V.fill (const Nothing)
          , cursorGain
          , b7IsWindowTouched: beatModSeven
          , b7WindowDims: beatModSeven
          }
      @|> doPreThirdVideo
  ThirdVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        ivoid $ create thirdVideoCreate
        cursorGain <- cursor (speaker This)
        moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
          $> { interpretVideo: interpretVideo d5 videoSpan
            , mostRecentWindowInteraction: V.fill (const Nothing)
            , cursorGain
            , videoSpan
            , b7WindowDims: beatModSeven
            }
        @|> doThirdVideo
  FourthVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        ivoid $ create fourthVideoCreate
        cursorGain <- cursor (speaker This)
        moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
          $> { mostRecentWindowInteraction: V.fill (const Nothing)
            , cursorGain
            , videoSpan
            , b7WindowDims: beatModSeven
            , rectangleSamba: moveVideo 0.0
            }
        @|> doFourthVideo
