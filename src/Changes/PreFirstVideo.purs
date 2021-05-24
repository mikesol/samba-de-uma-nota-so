module SambaDeUmaNotaSo.Changes.PreFirstVideo where

import Prelude
import Data.Array ((..))
import Data.Functor.Indexed (ivoid)
import Data.Int (toNumber)
import Data.Lens (_1, _2, over)
import Data.List (List)
import Data.List as L
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Math ((%))
import SambaDeUmaNotaSo.ASDR (quaverASR0)
import SambaDeUmaNotaSo.Constants (fourBeats)
import SambaDeUmaNotaSo.Env (modEnv)
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Instrumental0Paintings (halfBeat)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoGraph)
import SambaDeUmaNotaSo.Util (calcSlope, calcSlopeExp, mm01, (*!))
import WAGS.Change (change)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Graph.Optionals (gain_)
import WAGS.Graph.Parameter (AudioParameter_(..), AudioParameterTransition(..), defaultParam)

beatMap :: Array (Tuple Number Number)
beatMap = [ 0.0 /\ 0.0, 0.07 /\ 1.0, 0.1 /\ 0.3, 0.19 /\ 0.0 ]

beats :: List (Tuple Number Number)
beats = L.fromFoldable $ join $ map (\x -> map (over _1 (add x)) beatMap) (map (add fourBeats <<< mul halfBeat <<< toNumber) (0 .. 31))

changeGain :: forall proof. Tuple Number Number -> FrameSig PreFirstVideoGraph proof PreFirstVideoGraph Unit
changeGain (t /\ v) =
  ivoid
    $ change
        { chiffyE3Gain:
            gain_
              ( AudioParameter
                  { param: pure $ v * mm01 (calcSlope fourBeats 1.0 (fourBeats * 4.0) 0.0 t)
                  , timeOffset: t
                  , transition: LinearRamp
                  }
              )
        }

addBeats :: forall proof. List (Tuple Number Number) -> FrameSig PreFirstVideoGraph proof PreFirstVideoGraph Unit
addBeats L.Nil = WAGS.do
  pr <- proof
  withProof pr unit

addBeats (L.Cons a b) = WAGS.do
  changeGain a
  addBeats b

changesPreFirstVideo :: forall proof. FrameSig PreFirstVideoGraph proof PreFirstVideoGraph Unit
changesPreFirstVideo = WAGS.do
  changeGain (fourBeats /\ 0.0000000001)
  addBeats beats
