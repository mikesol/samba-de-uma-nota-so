module SambaDeUmaNotaSo.Loops.PreFirstVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (BaseGraph, EI0, EI1, MainBus, mainBus, mainBusFG)
import WAGS.Control.Types (Universe')
import WAGS.Graph.Constructors (Constant, Gain, Speaker)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)
import WAGS.Universe.AudioUnit (TConstant)
import WAGS.Universe.EdgeProfile (NoEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC)

type PreFirstVideoGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type PreFirstVideoUniverse cb
  = Universe' EI1 PreFirstVideoGraph cb

type PreFirstVideoLens constant
  = MainBus (constant (Constant GetSetAP))

preFirstVideo' ::
  forall dConstant.
  Decorating' dConstant ->
  PreFirstVideoLens dConstant
preFirstVideo' dConstant = mainBus (dConstant $ constant 0.0)

preFirstVideoMainBus :: Speaker (Focus (Gain GetSetAP (Constant GetSetAP)))
preFirstVideoMainBus = mainBusFG (constant 0.0)

preFirstVideoCreate = preFirstVideo' Identity :: PreFirstVideoLens Identity

preFirstVideoConstant = preFirstVideo' Focus :: PreFirstVideoLens Focus

deltaPreFirstVideo :: MainBus (Constant GetSetAP)
deltaPreFirstVideo = mainBus (constant 0.0)
