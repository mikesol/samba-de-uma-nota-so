module SambaDeUmaNotaSo.GetBuffers where

import Prelude
import Control.Promise (toAffE)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Heterogeneous.Folding (class FoldingWithIndex)
import WAGS.Interpret (BrowserAudioBuffer, AudioContext, decodeAudioDataFromUri)

data GetBuffersFoldingWithIndex
  = GetBuffersFoldingWithIndex

type GetBuffersFoldingWithIndexAcc
  = Tuple
      AudioContext
      (Array (Aff (Tuple String BrowserAudioBuffer)))

instance getBuffersFoldingWithIndex ::
  IsSymbol sym =>
  FoldingWithIndex
    GetBuffersFoldingWithIndex
    (proxy sym)
    GetBuffersFoldingWithIndexAcc
    String
    GetBuffersFoldingWithIndexAcc where
  foldingWithIndex GetBuffersFoldingWithIndex prop (Tuple audioCtx acc) toFetch =
    Tuple audioCtx
      ( acc
          <> pure
              ( Tuple (reflectSymbol prop)
                  <$> ( toAffE
                        $ decodeAudioDataFromUri
                            audioCtx
                            toFetch
                    )
              )
      )
