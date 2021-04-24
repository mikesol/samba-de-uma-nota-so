module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe)
import Effect (Effect)
import Graphics.Painting (Painting, Point)
import WAGS.Control.Thunkable (Thunkable)
import WAGS.Control.Types (FrameT, SceneT)
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
  = SceneT
      SambaSceneI
      FFIAudio
      (Effect Unit)
      proof
      Thunkable
      SambaRes

type StepSig step proof iu a
  = FrameT
      (SceneI SambaTrigger SambaWorld)
      FFIAudio
      (Effect Unit)
      proof
      Thunkable
      SambaRes
      iu
      step
      a ->
    SceneSig proof