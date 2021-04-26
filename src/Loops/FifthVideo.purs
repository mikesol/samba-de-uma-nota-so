module SambaDeUmaNotaSo.Loops.FifthVideo where

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

type FifthVideoGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type FifthVideoUniverse cb
  = Universe' EI1 FifthVideoGraph cb

type FifthVideoLens' :: forall k. (Type -> k) -> k
type FifthVideoLens' constant
  = constant (Constant GetSetAP)

type FifthVideoLens constant
  = MainBus (constant (Constant GetSetAP))

fifthVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FifthVideoLens' dConstant
fifthVideo'' f = f.dConstant $ constant 0.0

fifthVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FifthVideoLens dConstant
fifthVideo' f = mainBus (fifthVideo'' f)

fifthVideoMainBus :: MainBusFG (FifthVideoLens' Identity)
fifthVideoMainBus = mainBusFG (fifthVideo'' {dConstant : Identity})

fifthVideoCreate = fifthVideo' {dConstant : Identity} :: FifthVideoLens Identity

fifthVideoConstant = fifthVideo' {dConstant : Focus} :: FifthVideoLens Focus

deltaFifthVideo :: FifthVideoLens Identity
deltaFifthVideo = mainBus (Identity $ constant 0.0)
