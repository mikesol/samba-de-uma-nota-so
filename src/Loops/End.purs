module SambaDeUmaNotaSo.Loops.End where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (BaseGraph, EI0, MainBus, EI1, mainBus)
import WAGS.Control.Types (Universe')
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)
import WAGS.Universe.AudioUnit (TConstant)
import WAGS.Universe.EdgeProfile (NoEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC)

type EndGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type EndUniverse cb
  = Universe' EI1 EndGraph cb

type EndLens' :: (Type -> Type) -> Type
type EndLens' constant = constant (Constant GetSetAP)

type EndLens constant
  = MainBus (EndLens' constant)

endCreate' :: forall constant. Decorating' constant -> EndLens' constant
endCreate' dConstant = (dConstant $ constant 0.0) 
endCreate = endCreate' Identity :: EndLens' Identity

end' ::
  forall dConstant.
  Decorating' dConstant ->
  EndLens dConstant
end' dConstant = mainBus (endCreate' dConstant)

endFocusConstant = end' Focus :: EndLens Focus

deltaEnd :: MainBus (Constant GetSetAP)
deltaEnd = mainBus (constant 0.0)
