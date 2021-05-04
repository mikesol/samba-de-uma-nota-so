module SambaDeUmaNotaSo.IO.EighthVideo where

import Prelude
import Control.Comonad.Cofree (Cofree)
import Data.Maybe (Maybe, maybe)
import Data.Typelevel.Num (class Lt, class Nat, D1, D2, D3, D4, D5, D6, d0, d1, d2, d3, d4, d5)
import Data.Vec ((+>))
import Data.Vec as V
import Graphics.Painting (Point)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot, td2pt)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import SambaDeUmaNotaSo.Util (distance, scaleUnitPoint)
import Web.HTML.HTMLElement (DOMRect)

type HarmonyInfo
  = { pt :: Point, time :: Number }

data EighthVideoHarmony
  = NoSingers
  | OneSinger (V.Vec D1 HarmonyInfo)
  | TwoSingers (V.Vec D2 HarmonyInfo)
  | ThreeSingers (V.Vec D3 HarmonyInfo)
  | FourSingers (V.Vec D4 HarmonyInfo)
  | FiveSingers (V.Vec D5 HarmonyInfo)
  | SixSingers (V.Vec D6 HarmonyInfo)

nextEVH ::
  V.Vec D6 TouchedDot ->
  EighthVideoHarmony ->
  { time :: Number, pt :: Maybe Point, dr :: DOMRect } ->
  EighthVideoHarmony
nextEVH dotz x i = maybe x (go x <<< { time: i.time, dr: i.dr, pt: _ }) i.pt
  where
  go NoSingers { time, pt, dr } = if touching pt dr d0 then (OneSinger $ { time, pt } +> V.empty) else x

  go (OneSinger v) { time, pt, dr } = if touching pt dr d1 then (TwoSingers $ { time, pt } +> v) else x

  go (TwoSingers v) { time, pt, dr } = if touching pt dr d2 then (ThreeSingers $ { time, pt } +> v) else x

  go (ThreeSingers v) { time, pt, dr } = if touching pt dr d3 then (FourSingers $ { time, pt } +> v) else x

  go (FourSingers v) { time, pt, dr } = if touching pt dr d4 then (FiveSingers $ { time, pt } +> v) else x

  go (FiveSingers v) { time, pt, dr } = if touching pt dr d5 then (SixSingers $ { time, pt } +> v) else x

  go (SixSingers _) _ = x

  -- todo: is td2pt every time wasteful?
  -- as this only happens on screen touches, it's probably ok...
  -- maybe optimize it if those prove to be laggy
  touching :: forall n. Nat n => Lt n D6 => Point -> DOMRect -> n -> Boolean
  touching pt dr n = distance pt (scaleUnitPoint (td2pt (V.index dotz n)) dr) < ((min dr.width dr.height) / 15.0)

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
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , dotInteractions ::
        Cofree ((->) { time :: Number, pt :: Maybe Point, dr :: DOMRect }) EighthVideoHarmony
    , videoSpan :: VideoSpan
    , mainVideo :: TouchedDot
    }
