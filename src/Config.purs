module SambaDeUmaNotaSo.Config where

import SambaDeUmaNotaSo.Buffers (Buffers)

foreign import config ::
  { env :: String
  , audioBuffers :: Buffers String
  }
