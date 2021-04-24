module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude

import Effect (Effect)
import Graphics.Painting (Painting)
import WAGS.Control.Thunkable (Thunkable)
import WAGS.Control.Types (FrameT, SceneT)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

type SambaRes = { painting :: Painting }

type SambaTrigger = Unit
type SambaWorld = Unit

type SceneSig :: forall k. k -> Type
type SceneSig proof
  = SceneT
      (SceneI SambaTrigger SambaWorld)
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