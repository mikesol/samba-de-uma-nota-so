module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude

import Data.List (List)
import Effect (Effect)
import Graphics.Painting (Painting, Point)
import WAGS.Control.Thunkable (Thunkable)
import WAGS.Control.Types (FrameT, SceneT)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

type SambaRes = { painting :: Painting }

type SambaTrigger = { touches :: List Point }

type SambaWorld = { canvas :: { h :: Number , w :: Number } }

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