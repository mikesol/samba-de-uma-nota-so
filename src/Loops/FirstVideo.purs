module SambaDeUmaNotaSo.Loops.FirstVideo where

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

type FirstVideoGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type FirstVideoUniverse cb
  = Universe' EI1 FirstVideoGraph cb

type FirstVideoLens' :: forall k. (Type -> k) -> k
type FirstVideoLens' constant
  = constant (Constant GetSetAP)

type FirstVideoLens constant
  = MainBus (constant (Constant GetSetAP))

firstVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FirstVideoLens' dConstant
firstVideo'' f = f.dConstant $ constant 0.0

firstVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FirstVideoLens dConstant
firstVideo' f = mainBus (firstVideo'' f)

firstVideoMainBus :: MainBusFG (FirstVideoLens' Identity)
firstVideoMainBus = mainBusFG (firstVideo'' {dConstant : Identity})

firstVideoCreate = firstVideo' {dConstant : Identity} :: FirstVideoLens Identity

firstVideoConstant = firstVideo' {dConstant : Focus} :: FirstVideoLens Focus

deltaFirstVideo :: FirstVideoLens Identity
deltaFirstVideo = mainBus (Identity $ constant 0.0)
