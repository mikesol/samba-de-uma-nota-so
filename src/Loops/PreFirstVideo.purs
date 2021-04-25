module SambaDeUmaNotaSo.Loops.PreFirstVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (BaseGraph, EI0, EI1, MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Control.Types (Universe')
import WAGS.Graph.Constructors (Constant)
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

type PreFirstVideoLens' :: forall k. (Type -> k) -> k
type PreFirstVideoLens' constant
  = constant (Constant GetSetAP)

type PreFirstVideoLens constant
  = MainBus (constant (Constant GetSetAP))

preFirstVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreFirstVideoLens' dConstant
preFirstVideo'' f = f.dConstant $ constant 0.0

preFirstVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreFirstVideoLens dConstant
preFirstVideo' f = mainBus (preFirstVideo'' f)

preFirstVideoMainBus :: MainBusFG (PreFirstVideoLens' Identity)
preFirstVideoMainBus = mainBusFG (preFirstVideo'' {dConstant : Identity})

preFirstVideoCreate = preFirstVideo' {dConstant : Identity} :: PreFirstVideoLens Identity

preFirstVideoConstant = preFirstVideo' {dConstant : Focus} :: PreFirstVideoLens Focus

deltaPreFirstVideo :: PreFirstVideoLens Identity
deltaPreFirstVideo = mainBus (Identity $ constant 0.0)
