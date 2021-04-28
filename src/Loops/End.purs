module SambaDeUmaNotaSo.Loops.End where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (MainBus, mainBus)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)


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
