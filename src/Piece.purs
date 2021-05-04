module SambaDeUmaNotaSo.Piece where

import Prelude
import Control.Comonad.Cofree ((:<))
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d3, d5)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import SambaDeUmaNotaSo.Constants (fourMeasures, twoMeasures)
import SambaDeUmaNotaSo.Drawing (blackBackground)
import SambaDeUmaNotaSo.FrameSig (SambaSceneI, SceneSig, SambaRes)
import SambaDeUmaNotaSo.IO.EighthVideo (EighthVideoHarmony(..), nextEVH)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideo, isVideoWindowTouched)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot(..), td2harmChain)
import SambaDeUmaNotaSo.Loops.AwaitingEighthVideo (awaitingEighthVideoCreate)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (awaitingFirstVideoCreate)
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (awaitingSecondVideoCreate)
import SambaDeUmaNotaSo.Loops.EighthVideo (eighthVideoCreate)
import SambaDeUmaNotaSo.Loops.FifthVideo (fifthVideoCreate)
import SambaDeUmaNotaSo.Loops.FirstVideo (firstVideoCreate)
import SambaDeUmaNotaSo.Loops.FourthVideo (fourthVideoCreate)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (preFirstVideoCreate)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (preSecondVideoCreate)
import SambaDeUmaNotaSo.Loops.PreThirdVideo (preThirdVideoCreate)
import SambaDeUmaNotaSo.Loops.SecondVideo (secondVideoCreate)
import SambaDeUmaNotaSo.Loops.SeventhVideo (seventhVideoCreate)
import SambaDeUmaNotaSo.Loops.SixthVideo (sixthVideoCreate)
import SambaDeUmaNotaSo.Loops.ThirdVideo (thirdVideoCreate)
import SambaDeUmaNotaSo.Transitions.AwaitingEighthVideo (doAwaitingEighthVideo, dotInteractions)
import SambaDeUmaNotaSo.Transitions.AwaitingFirstVideo (doAwaitingFirstVideo)
import SambaDeUmaNotaSo.Transitions.AwaitingSecondVideo (doAwaitingSecondVideo)
import SambaDeUmaNotaSo.Transitions.EighthVideo (doEighthVideo, instrumentalAnimation)
import SambaDeUmaNotaSo.Transitions.FifthVideo (doFifthVideo, quaseNada)
import SambaDeUmaNotaSo.Transitions.FirstVideo (doFirstVideo)
import SambaDeUmaNotaSo.Transitions.FourthVideo (doFourthVideo, quantaGenteExiste)
import SambaDeUmaNotaSo.Transitions.PreFirstVideo (doPreFirstVideo)
import SambaDeUmaNotaSo.Transitions.PreSecondVideo (doPreSecondVideo)
import SambaDeUmaNotaSo.Transitions.PreThirdVideo (doPreThirdVideo)
import SambaDeUmaNotaSo.Transitions.SecondVideo (doSecondVideo)
import SambaDeUmaNotaSo.Transitions.SeventhVideo (doSeventhVideo)
import SambaDeUmaNotaSo.Transitions.SixthVideo (deTodaAEscala, doSixthVideo, dotMover, seventhVideoLoop)
import SambaDeUmaNotaSo.Transitions.ThirdVideo (doThirdVideo, rectangleSamba)
import SambaDeUmaNotaSo.Transitions.ToInstrumental (doToInstrumental)
import SambaDeUmaNotaSo.Util (BeatMod7', beatModSeven)
import WAGS.Control.Functions (env, modifyRes, start, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame0, InitialFrameT)
import WAGS.Interpret (class AudioInterpret)

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
  | FifthVideo
  | SixthVideo
  | SeventhVideo
  | AwaitingEighthVideo
  | EighthVideo
  | ToInstrumental

startAt = FifthVideo :: StartAt

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
      preFirstVideoCreate
        $> { nTouchesSoFar: 0
          , mostRecentWindowInteraction: V.fill (const Nothing)
          }
      @|> doPreFirstVideo
  AwaitingFirstVideo ->
    WAGS.do
      startWithBlackBackground
      awaitingFirstVideoCreate
        $> { interpretVideo: interpretVideo d3
          , isVideoWindowTouched: isVideoWindowTouched d3
          , mostRecentWindowInteraction: V.fill (const Nothing)
          }
      @|> doAwaitingFirstVideo
  FirstVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        firstVideoCreate
          $> { interpretVideo: interpretVideo d3 videoSpan
            , mostRecentWindowInteraction: V.fill (const Nothing)
            , videoSpan
            }
        @|> doFirstVideo
  PreSecondVideo ->
    WAGS.do
      startWithBlackBackground
      preSecondVideoCreate
        $> { nTouchesSoFar: 0
          , mostRecentWindowInteraction: V.fill (const Nothing)
          }
      @|> doPreSecondVideo
  AwaitingSecondVideo ->
    WAGS.do
      startWithBlackBackground
      awaitingSecondVideoCreate
        $> { interpretVideo: interpretVideo d5
          , isVideoWindowTouched: isVideoWindowTouched d5
          , mostRecentWindowInteraction: V.fill (const Nothing)
          }
      @|> doAwaitingSecondVideo
  SecondVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        secondVideoCreate
          $> { interpretVideo: interpretVideo d5 videoSpan
            , mostRecentWindowInteraction: V.fill (const Nothing)
            , videoSpan
            }
        @|> doSecondVideo
  PreThirdVideo ->
    WAGS.do
      startWithBlackBackground
      preThirdVideoCreate
        $> { mostRecentWindowInteraction: V.fill (const Nothing)
          , b7IsWindowTouched: beatModSeven 0.0 :: BeatMod7' Boolean
          , b7WindowDims: beatModSeven 0.0 :: BeatMod7' Rectangle
          }
      @|> doPreThirdVideo
  ThirdVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        thirdVideoCreate
          $> { interpretVideo: interpretVideo d5 videoSpan
            , mostRecentWindowInteraction: V.fill (const Nothing)
            , videoSpan
            , b7WindowDims: beatModSeven 0.0 :: BeatMod7' Rectangle
            }
        @|> doThirdVideo
  FourthVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        fourthVideoCreate
          $> { mostRecentWindowInteraction: V.fill (const Nothing)
            , videoSpan
            , b7WindowDims: beatModSeven 0.0 :: BeatMod7' Rectangle
            , rectangleSamba: rectangleSamba 0.0
            }
        @|> doFourthVideo
  FifthVideo ->
    let
      videoSpan = { start: 0.0, end: twoMeasures }
    in
      WAGS.do
        startWithBlackBackground
        fifthVideoCreate
          $> { videoSpan
            , quantaGenteExiste: quantaGenteExiste videoSpan.start
            }
        @|> doFifthVideo
  SixthVideo ->
    let
      videoSpan = { start: 0.0, end: twoMeasures }
    in
      WAGS.do
        startWithBlackBackground
        sixthVideoCreate
          $> { videoSpan
            , quaseNada: quaseNada videoSpan.start
            }
        @|> doSixthVideo
  SeventhVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        seventhVideoCreate
          $> { videoSpan
            , deTodaAEscala: deTodaAEscala videoSpan.start
            , seventhVideoLoop: seventhVideoLoop videoSpan.start
            , dotMover: dotMover videoSpan.start
            }
        @|> doSeventhVideo
  AwaitingEighthVideo ->
    WAGS.do
      startWithBlackBackground
      awaitingEighthVideoCreate
        $> { dotMover: dotMover 0.0 }
      @|> doAwaitingEighthVideo
  EighthVideo ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        eighthVideoCreate
          $> { videoSpan
            , mostRecentWindowInteraction: V.fill $ const Nothing
            , dotInteractions: dotInteractions TDThree
            , mainVideo: TDThree
            }
        @|> doEighthVideo
  ToInstrumental ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        eighthVideoCreate
          $> { videoSpan
            , mostRecentWindowInteraction: V.fill $ const Nothing
            , dotInteractions: dotInteractions TDThree
            , mainVideo: TDThree
            , instrumentalAnimation: instrumentalAnimation 0.0
            }
        @|> doToInstrumental
