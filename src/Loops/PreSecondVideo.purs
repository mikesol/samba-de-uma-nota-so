module SambaDeUmaNotaSo.Loops.PreSecondVideo where

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

type PreSecondVideoGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type PreSecondVideoUniverse cb
  = Universe' EI1 PreSecondVideoGraph cb

type PreSecondVideoLens' :: forall k. (Type -> k) -> k
type PreSecondVideoLens' constant
  = constant (Constant GetSetAP)

type PreSecondVideoLens constant
  = MainBus (constant (Constant GetSetAP))

preSecondVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreSecondVideoLens' dConstant
preSecondVideo'' f = f.dConstant $ constant 0.0

preSecondVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreSecondVideoLens dConstant
preSecondVideo' f = mainBus (preSecondVideo'' f)

preSecondVideoMainBus :: MainBusFG (PreSecondVideoLens' Identity)
preSecondVideoMainBus = mainBusFG (preSecondVideo'' {dConstant : Identity})

preSecondVideoCreate = preSecondVideo' {dConstant : Identity} :: PreSecondVideoLens Identity

preSecondVideoConstant = preSecondVideo' {dConstant : Focus} :: PreSecondVideoLens Focus

deltaPreSecondVideo :: PreSecondVideoLens Identity
deltaPreSecondVideo = mainBus (Identity $ constant 0.0)
