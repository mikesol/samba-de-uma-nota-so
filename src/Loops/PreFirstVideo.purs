module SambaDeUmaNotaSo.Loops.PreFirstVideo where

import Prelude

import Control.Apply.Indexed ((:*>))
import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (MainBus, mainBus)
import Type.Data.Peano as N
import Type.Proxy (Proxy(..))
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)
import WAGS.MoveNode (moveNode)

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

preFirstVideoCreate' = preFirstVideo' {dConstant : Identity} :: PreFirstVideoLens Identity

preFirstVideoCreate =   create preFirstVideoCreate' :*>  moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
