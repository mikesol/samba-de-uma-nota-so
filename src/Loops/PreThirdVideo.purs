module SambaDeUmaNotaSo.Loops.PreThirdVideo where

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

type PreThirdVideoGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type PreThirdVideoUniverse cb
  = Universe' EI1 PreThirdVideoGraph cb

type PreThirdVideoLens' :: forall k. (Type -> k) -> k
type PreThirdVideoLens' constant
  = constant (Constant GetSetAP)

type PreThirdVideoLens constant
  = MainBus (constant (Constant GetSetAP))

preThirdVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreThirdVideoLens' dConstant
preThirdVideo'' f = f.dConstant $ constant 0.0

preThirdVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreThirdVideoLens dConstant
preThirdVideo' f = mainBus (preThirdVideo'' f)

preThirdVideoMainBus :: MainBusFG (PreThirdVideoLens' Identity)
preThirdVideoMainBus = mainBusFG (preThirdVideo'' {dConstant : Identity})

preThirdVideoCreate = preThirdVideo' {dConstant : Identity} :: PreThirdVideoLens Identity

preThirdVideoConstant = preThirdVideo' {dConstant : Focus} :: PreThirdVideoLens Focus

deltaPreThirdVideo :: PreThirdVideoLens Identity
deltaPreThirdVideo = mainBus (Identity $ constant 0.0)
