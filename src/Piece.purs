module SambaDeUmaNotaSo.Piece where

import Prelude
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d3, d4, d5)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import SambaDeUmaNotaSo.Config (config)
import SambaDeUmaNotaSo.Constants (eightMeasures, fourMeasures, twoMeasures)
import SambaDeUmaNotaSo.Drawing (blackBackground)
import SambaDeUmaNotaSo.FrameSig (SambaSceneI, SceneSig, SambaRes)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideo, interpretVideoNoDim, isVideoWindowTouched)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot(..))
import SambaDeUmaNotaSo.Instrumental0Paintings (instrumental0Painting)
import SambaDeUmaNotaSo.Instrumental1Paintings (instrumental1Painting)
import SambaDeUmaNotaSo.Loops.AwaitingEighthVideo (awaitingEighthVideoCreate)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (awaitingFirstVideoCreate)
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (awaitingSecondVideoCreate)
import SambaDeUmaNotaSo.Loops.Coda0 (coda0Create)
import SambaDeUmaNotaSo.Loops.Coda1 (coda1Create)
import SambaDeUmaNotaSo.Loops.EighthVideo (eighthVideoCreate)
import SambaDeUmaNotaSo.Loops.FifthVideo (fifthVideoCreate)
import SambaDeUmaNotaSo.Loops.FirstVideo (firstVideoCreate)
import SambaDeUmaNotaSo.Loops.FourthVideo (fourthVideoCreate)
import SambaDeUmaNotaSo.Loops.Instrumental0 (instrumental0Create)
import SambaDeUmaNotaSo.Loops.Instrumental1 (instrumental1Create)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (preFirstVideoCreate)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (preSecondVideoCreate)
import SambaDeUmaNotaSo.Loops.PreThirdVideo (preThirdVideoCreate)
import SambaDeUmaNotaSo.Loops.SecondVideo (secondVideoCreate)
import SambaDeUmaNotaSo.Loops.SeventhVideo (seventhVideoCreate)
import SambaDeUmaNotaSo.Loops.SixthVideo (sixthVideoCreate)
import SambaDeUmaNotaSo.Loops.ThirdVideo (thirdVideoCreate)
import SambaDeUmaNotaSo.ToInstrumentalWedges (instrumentalAnimation)
import SambaDeUmaNotaSo.Transitions.AwaitingEighthVideo (doAwaitingEighthVideo, dotInteractions)
import SambaDeUmaNotaSo.Transitions.AwaitingFirstVideo (doAwaitingFirstVideo)
import SambaDeUmaNotaSo.Transitions.AwaitingSecondVideo (doAwaitingSecondVideo)
import SambaDeUmaNotaSo.Transitions.Coda0 (codaSamba, doCoda0)
import SambaDeUmaNotaSo.Transitions.Coda1 (doCoda1)
import SambaDeUmaNotaSo.Transitions.EighthVideo (doEighthVideo)
import SambaDeUmaNotaSo.Transitions.FifthVideo (doFifthVideo, quaseNada)
import SambaDeUmaNotaSo.Transitions.FirstVideo (doFirstVideo)
import SambaDeUmaNotaSo.Transitions.FourthVideo (doFourthVideo, quantaGenteExiste)
import SambaDeUmaNotaSo.Transitions.Instrumental0 (doInstrumental0, startingOnOff)
import SambaDeUmaNotaSo.Transitions.Instrumental1 (doInstrumental1)
import SambaDeUmaNotaSo.Transitions.PreFirstVideo (doPreFirstVideo)
import SambaDeUmaNotaSo.Transitions.PreSecondVideo (doPreSecondVideo)
import SambaDeUmaNotaSo.Transitions.PreThirdVideo (doPreThirdVideo)
import SambaDeUmaNotaSo.Transitions.SecondVideo (doSecondVideo)
import SambaDeUmaNotaSo.Transitions.SeventhVideo (doSeventhVideo)
import SambaDeUmaNotaSo.Transitions.SixthVideo (deTodaAEscala, doSixthVideo, dotMover, seventhVideoLoop)
import SambaDeUmaNotaSo.Transitions.ThirdVideo (doThirdVideo, rectangleSamba)
import SambaDeUmaNotaSo.Transitions.ToInstrumental (doToInstrumental, startingActiveZones)
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
  | Instrumental0
  | Instrumental1
  | Coda0
  | Coda1

startAt :: StartAt
startAt = if config.env == "production" then PreFirstVideo else SixthVideo

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
  Instrumental0 ->
    let
      videoSpan = { start: 0.0, end: eightMeasures }
    in
      WAGS.do
        startWithBlackBackground
        instrumental0Create
          $> { videoSpan
            , activeZones: startingActiveZones
            , instruments: instrumental0Painting 0.0
            }
        @|> doInstrumental0
  Instrumental1 ->
    let
      videoSpan = { start: 0.0, end: eightMeasures }
    in
      WAGS.do
        startWithBlackBackground
        instrumental1Create
          $> { videoSpan
            , onOff: startingOnOff
            , instruments: instrumental1Painting 0.0
            , mostRecentWindowInteraction: V.fill (const Nothing)
            }
        @|> doInstrumental1
  Coda0 ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        coda0Create
          $> { mostRecentWindowInteraction: V.fill (const Nothing)
            , interpretVideo: interpretVideoNoDim d4 videoSpan -- d4 choisi au pif...
            , videoSpan: videoSpan
            }
        @|> doCoda0
  Coda1 ->
    let
      videoSpan = { start: 0.0, end: fourMeasures }
    in
      WAGS.do
        startWithBlackBackground
        coda1Create
          $> { mostRecentWindowInteraction: V.fill (const Nothing)
            , videoSpan: videoSpan
            , codaSamba: codaSamba videoSpan.start
            }
        @|> doCoda1
