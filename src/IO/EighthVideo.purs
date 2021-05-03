module SambaDeUmaNotaSo.IO.EighthVideo where

import Control.Comonad.Cofree (Cofree)
import Data.Maybe (Maybe)
import Data.Typelevel.Num (D1, D2, D3, D4, D5, D6)
import Data.Vec as V
import Graphics.Painting (Point)
import SambaDeUmaNotaSo.IO.PreFirstVideo (InterpretVideoSig0)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)

type HarmonyInfo
  = { x :: Number, y :: Number, time :: Number }

data EighthVideoHarmony
  = NoSingers
  | OneSinger (V.Vec D1 HarmonyInfo)
  | TwoSingers (V.Vec D2 HarmonyInfo)
  | ThreeSingers (V.Vec D3 HarmonyInfo)
  | FourSingers (V.Vec D4 HarmonyInfo)
  | FiveSingers (V.Vec D5 HarmonyInfo)
  | SixSingers (V.Vec D6 HarmonyInfo)

harmonyToVec :: forall a. (forall n. V.Vec n HarmonyInfo -> a) -> EighthVideoHarmony -> a
harmonyToVec f = case _ of
  NoSingers -> f V.empty
  OneSinger v -> f v
  TwoSingers v -> f v
  ThreeSingers v -> f v
  FourSingers v -> f v
  FiveSingers v -> f v
  SixSingers v -> f v

-- | We go back to the normal player
type Accumulator
  = { interpretVideo :: InterpretVideoSig0
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , dotInteractions :: Cofree ((->) { time :: Number, point :: Maybe Point }) EighthVideoHarmony
    , videoSpan :: VideoSpan
    }
