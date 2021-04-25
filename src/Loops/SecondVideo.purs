module SambaDeUmaNotaSo.Loops.SecondVideo where

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

type SecondVideoGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type SecondVideoUniverse cb
  = Universe' EI1 SecondVideoGraph cb

type SecondVideoLens' :: forall k. (Type -> k) -> k
type SecondVideoLens' constant
  = constant (Constant GetSetAP)

type SecondVideoLens constant
  = MainBus (constant (Constant GetSetAP))

secondVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  SecondVideoLens' dConstant
secondVideo'' f = f.dConstant $ constant 0.0

secondVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  SecondVideoLens dConstant
secondVideo' f = mainBus (secondVideo'' f)

secondVideoMainBus :: MainBusFG (SecondVideoLens' Identity)
secondVideoMainBus = mainBusFG (secondVideo'' {dConstant : Identity})

secondVideoCreate = secondVideo' {dConstant : Identity} :: SecondVideoLens Identity

secondVideoConstant = secondVideo' {dConstant : Focus} :: SecondVideoLens Focus

deltaSecondVideo :: SecondVideoLens Identity
deltaSecondVideo = mainBus (Identity $ constant 0.0)
