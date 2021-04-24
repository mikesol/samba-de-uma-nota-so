module SambaDeUmaNotaSo.Piece where

import Prelude
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Vec as V
import SambaDeUmaNotaSo.Loops.PreFirstVideo (preFirstVideoCreate, preFirstVideoMainBus)
import SambaDeUmaNotaSo.Transitions.PreFirstVideo (doPreFirstVideo)
import Type.Data.Peano as N
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (start, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame0)
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Example.KitchenSink.TLP.LoopSig (SceneSig)
import WAGS.MoveNode (moveNode)

piece :: SceneSig Frame0
piece =
  WAGS.do
    start
    ivoid $ create preFirstVideoCreate
    cursorGain <- cursor preFirstVideoMainBus
    moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0)
      $> { nTouchesSoFar: 0
        , mostRecentWindowInteraction: V.fill (const Nothing)
        , cursorGain
        }
    @|> doPreFirstVideo
