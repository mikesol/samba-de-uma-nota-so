module SambaDeUmaNotaSo.IO.EighthVideo where

import Prelude
import Control.Comonad.Cofree (Cofree)
import Control.Plus (empty)
import Data.Maybe (Maybe, maybe)
import Data.Typelevel.Num (D1, D2, D3, D4, D5, D6, d0, d1, d2, d3, d4, d5)
import Data.Vec ((+>))
import Data.Vec as V
import Graphics.Painting (Point)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot, td2pt)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import SambaDeUmaNotaSo.Util (distance, scaleUnitPoint)
import Web.HTML.HTMLElement (DOMRect)

type HarmonyInfo
  = { td :: TouchedDot, time :: Number }

data EighthVideoHarmony
  = NoSingers TouchedDot
  | OneSinger TouchedDot (V.Vec D1 HarmonyInfo)
  | TwoSingers TouchedDot (V.Vec D2 HarmonyInfo)
  | ThreeSingers TouchedDot (V.Vec D3 HarmonyInfo)
  | FourSingers TouchedDot (V.Vec D4 HarmonyInfo)
  | FiveSingers TouchedDot (V.Vec D5 HarmonyInfo)
  | SixSingers (V.Vec D6 HarmonyInfo)

nextEVH ::
  V.Vec D6 TouchedDot ->
  EighthVideoHarmony ->
  { time :: Number, pt :: Maybe Point, dr :: DOMRect } ->
  EighthVideoHarmony
nextEVH dotz x i = maybe x (go x <<< { time: i.time, dr: i.dr, pt: _ }) i.pt
  where
  go (NoSingers _) { time, pt, dr } = let td = V.index dotz d0 in (if touching pt dr td then (OneSinger (V.index dotz d1) $ { time, td } +> V.empty) else x)

  go (OneSinger _ v) { time, pt, dr } = let td = V.index dotz d1 in (if touching pt dr td then (TwoSingers (V.index dotz d2) $ { time, td } +> v) else x)

  go (TwoSingers _ v) { time, pt, dr } = let td = V.index dotz d2 in (if touching pt dr td then (ThreeSingers (V.index dotz d3) $ { time, td } +> v) else x)

  go (ThreeSingers _ v) { time, pt, dr } = let td = V.index dotz d3 in (if touching pt dr td then (FourSingers (V.index dotz d4) $ { time, td } +> v) else x)

  go (FourSingers _ v) { time, pt, dr } = let td = V.index dotz d4 in (if touching pt dr td then (FiveSingers (V.index dotz d5) $ { time, td } +> v) else x)

  go (FiveSingers _ v) { time, pt, dr } = let td = V.index dotz d5 in (if touching pt dr td then (SixSingers $ { time, td } +> v) else x)

  go (SixSingers _) _ = x

  -- todo: is td2pt every time wasteful?
  -- as this only happens on screen touches, it's probably ok...
  -- maybe optimize it if those prove to be laggy
  touching :: Point -> DOMRect -> TouchedDot -> Boolean
  touching pt dr td = distance pt (scaleUnitPoint (td2pt td) dr) < ((min dr.width dr.height) / 15.0)

harmonyToVec :: forall a. (forall n. V.Vec n HarmonyInfo -> a) -> EighthVideoHarmony -> a
harmonyToVec f = case _ of
  NoSingers _ -> f V.empty
  OneSinger _ v -> f v
  TwoSingers _ v -> f v
  ThreeSingers _ v -> f v
  FourSingers _ v -> f v
  FiveSingers _ v -> f v
  SixSingers v -> f v

harmonyToNext :: EighthVideoHarmony -> Maybe TouchedDot
harmonyToNext = case _ of
  NoSingers v -> pure v
  OneSinger v _ -> pure v
  TwoSingers v _ -> pure v
  ThreeSingers v _ -> pure v
  FourSingers v _ -> pure v
  FiveSingers v _ -> pure v
  SixSingers _ -> empty

type FDotInteractions
  = (->) { time :: Number, pt :: Maybe Point, dr :: DOMRect }

type DotInteractions
  = FDotInteractions (Cofree FDotInteractions EighthVideoHarmony)

-- | We go back to the normal player
type Accumulator
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , dotInteractions :: DotInteractions
    , videoSpan :: VideoSpan
    , mainVideo :: TouchedDot
    }
