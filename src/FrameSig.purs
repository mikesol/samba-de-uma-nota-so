module SambaDeUmaNotaSo.FrameSig where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe)
import Effect (Effect)
import Graphics.Painting (Painting, Point)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Scene, WAG)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)
import Web.HTML.HTMLElement (DOMRect)

type SambaRes = { painting :: Painting }

asTouch :: SambaTrigger -> Maybe Point
asTouch = case _ of
  Interaction { touch } -> pure touch
  _ -> empty

data SambaTrigger = Start | Interaction { touch :: Point }

type SambaWorld = { canvas :: DOMRect }

type SambaSceneI = SceneI SambaTrigger SambaWorld

type SceneSig :: forall k. k -> Type
type SceneSig proof
  = Scene
      SambaSceneI
      FFIAudio
      (Effect Unit)
      proof
      SambaRes

type WAGSig step proof a
  = WAG
      FFIAudio
      (Effect Unit)
      proof
      SambaRes
      step
      a

type IxWAGSig stepA stepB proof a
  = IxWAG
      FFIAudio
      (Effect Unit)
      proof
      SambaRes
      stepA
      stepB
      a

type StepSig step proof a
  = WAGSig step proof a -> SceneSig proof