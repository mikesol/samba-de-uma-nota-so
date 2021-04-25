module SambaDeUmaNotaSo.Loops.FourthVideo where

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

type FourthVideoGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type FourthVideoUniverse cb
  = Universe' EI1 FourthVideoGraph cb

type FourthVideoLens' :: forall k. (Type -> k) -> k
type FourthVideoLens' constant
  = constant (Constant GetSetAP)

type FourthVideoLens constant
  = MainBus (constant (Constant GetSetAP))

fourthVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FourthVideoLens' dConstant
fourthVideo'' f = f.dConstant $ constant 0.0

fourthVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FourthVideoLens dConstant
fourthVideo' f = mainBus (fourthVideo'' f)

fourthVideoMainBus :: MainBusFG (FourthVideoLens' Identity)
fourthVideoMainBus = mainBusFG (fourthVideo'' {dConstant : Identity})

fourthVideoCreate = fourthVideo' {dConstant : Identity} :: FourthVideoLens Identity

fourthVideoConstant = fourthVideo' {dConstant : Focus} :: FourthVideoLens Focus

deltaFourthVideo :: FourthVideoLens Identity
deltaFourthVideo = mainBus (Identity $ constant 0.0)
