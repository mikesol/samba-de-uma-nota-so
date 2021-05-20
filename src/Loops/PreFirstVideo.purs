module SambaDeUmaNotaSo.Loops.PreFirstVideo where

import Prelude
import Data.Tuple.Nested (type (/\))
import Type.Row (type (+))
import WAGS.Create (create)
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import WAGS.Graph.AudioUnit (TGain, THighpass, TLoopBuf, TSpeaker, TStereoPanner)
import WAGS.Graph.Optionals (gain, highpass, loopBuf, pan, speaker)

-- chiffyE1, chiffyE2, chiffyE3, chiffyE4, chiffyE5 all pure sounding
-- digistensuE2, digistensuE3, digistensuE4, digistensuE5 all morph. interesting to interrupt them.
-- jarbleIctusE3 may work for receding into the distance
-- mtDillJabsE3, mtDillJabsE4, mtDillJabsE5 are all in rhythm, may work or not. could also wind up being too busy.
-- shamisenE3, shamisenE4, shamisenE5, shamisenE6 are nice for plucked sounds
-- tinefullE0, tinefullE1, tinefullE2, tinefullE3, tinefullE4, tinefullE5 nice and nostalgic
-- vsMagicE1, vsMagicE2, vsMagicE3, vsMagicE4, vsMagicE5 very rich, probably makes sense in spurts but otherwise too gimmicky for a sustain
type ChiffyE1 r
  = ( chiffyE1Gain :: TGain /\ { chiffyE1Pan :: Unit }
    , chiffyE1Pan :: TStereoPanner /\ { chiffyE1HPF :: Unit }
    , chiffyE1HPF :: THighpass /\ { chiffyE1Buf :: Unit }
    , chiffyE1Buf :: TLoopBuf /\ {}
    | r
    )

type ChiffyE5 r
  = ( chiffyE5Gain :: TGain /\ { chiffyE5Pan :: Unit }
    , chiffyE5Pan :: TStereoPanner /\ { chiffyE5HPF :: Unit }
    , chiffyE5HPF :: THighpass /\ { chiffyE5Buf :: Unit }
    , chiffyE5Buf :: TLoopBuf /\ {}
    | r
    )

type DigistensuE5 r
  = ( digistensuE5Gain :: TGain /\ { digistensuE5Pan :: Unit }
    , digistensuE5Pan :: TStereoPanner /\ { digistensuE5HPF :: Unit }
    , digistensuE5HPF :: THighpass /\ { digistensuE5Buf :: Unit }
    , digistensuE5Buf :: TLoopBuf /\ {}
    | r
    )

type MtDillJabsE4 r
  = ( mtDillJabsE4Gain :: TGain /\ { mtDillJabsE4Pan :: Unit }
    , mtDillJabsE4Pan :: TStereoPanner /\ { mtDillJabsE4HPF :: Unit }
    , mtDillJabsE4HPF :: THighpass /\ { mtDillJabsE4Buf :: Unit }
    , mtDillJabsE4Buf :: TLoopBuf /\ {}
    | r
    )

type ShamisenE5 r
  = ( shamisenE5Gain :: TGain /\ { shamisenE5Pan :: Unit }
    , shamisenE5Pan :: TStereoPanner /\ { shamisenE5HPF :: Unit }
    , shamisenE5HPF :: THighpass /\ { shamisenE5Buf :: Unit }
    , shamisenE5Buf :: TLoopBuf /\ {}
    | r
    )

type TinefullE2 r
  = ( tinefullE2Gain :: TGain /\ { tinefullE2Pan :: Unit }
    , tinefullE2Pan :: TStereoPanner /\ { tinefullE2HPF :: Unit }
    , tinefullE2HPF :: THighpass /\ { tinefullE2Buf :: Unit }
    , tinefullE2Buf :: TLoopBuf /\ {}
    | r
    )

type VsMagicE1 r
  = ( vsMagicE1Gain :: TGain /\ { vsMagicE1Pan :: Unit }
    , vsMagicE1Pan :: TStereoPanner /\ { vsMagicE1HPF :: Unit }
    , vsMagicE1HPF :: THighpass /\ { vsMagicE1Buf :: Unit }
    , vsMagicE1Buf :: TLoopBuf /\ {}
    | r
    )

type VsMagicE5 r
  = ( vsMagicE5Gain :: TGain /\ { vsMagicE5Pan :: Unit }
    , vsMagicE5Pan :: TStereoPanner /\ { vsMagicE5HPF :: Unit }
    , vsMagicE5HPF :: THighpass /\ { vsMagicE5Buf :: Unit }
    , vsMagicE5Buf :: TLoopBuf /\ {}
    | r
    )

type ChiffyE2 r
  = ( chiffyE2Gain :: TGain /\ { chiffyE2Pan :: Unit }
    , chiffyE2Pan :: TStereoPanner /\ { chiffyE2HPF :: Unit }
    , chiffyE2HPF :: THighpass /\ { chiffyE2Buf :: Unit }
    , chiffyE2Buf :: TLoopBuf /\ {}
    | r
    )

type DigistensuE2 r
  = ( digistensuE2Gain :: TGain /\ { digistensuE2Pan :: Unit }
    , digistensuE2Pan :: TStereoPanner /\ { digistensuE2HPF :: Unit }
    , digistensuE2HPF :: THighpass /\ { digistensuE2Buf :: Unit }
    , digistensuE2Buf :: TLoopBuf /\ {}
    | r
    )

type DigistensuE6 r
  = ( digistensuE6Gain :: TGain /\ { digistensuE6Pan :: Unit }
    , digistensuE6Pan :: TStereoPanner /\ { digistensuE6HPF :: Unit }
    , digistensuE6HPF :: THighpass /\ { digistensuE6Buf :: Unit }
    , digistensuE6Buf :: TLoopBuf /\ {}
    | r
    )

type MtDillJabsE5 r
  = ( mtDillJabsE5Gain :: TGain /\ { mtDillJabsE5Pan :: Unit }
    , mtDillJabsE5Pan :: TStereoPanner /\ { mtDillJabsE5HPF :: Unit }
    , mtDillJabsE5HPF :: THighpass /\ { mtDillJabsE5Buf :: Unit }
    , mtDillJabsE5Buf :: TLoopBuf /\ {}
    | r
    )

type ShamisenE6 r
  = ( shamisenE6Gain :: TGain /\ { shamisenE6Pan :: Unit }
    , shamisenE6Pan :: TStereoPanner /\ { shamisenE6HPF :: Unit }
    , shamisenE6HPF :: THighpass /\ { shamisenE6Buf :: Unit }
    , shamisenE6Buf :: TLoopBuf /\ {}
    | r
    )

type TinefullE3 r
  = ( tinefullE3Gain :: TGain /\ { tinefullE3Pan :: Unit }
    , tinefullE3Pan :: TStereoPanner /\ { tinefullE3HPF :: Unit }
    , tinefullE3HPF :: THighpass /\ { tinefullE3Buf :: Unit }
    , tinefullE3Buf :: TLoopBuf /\ {}
    | r
    )

type VsMagicE2 r
  = ( vsMagicE2Gain :: TGain /\ { vsMagicE2Pan :: Unit }
    , vsMagicE2Pan :: TStereoPanner /\ { vsMagicE2HPF :: Unit }
    , vsMagicE2HPF :: THighpass /\ { vsMagicE2Buf :: Unit }
    , vsMagicE2Buf :: TLoopBuf /\ {}
    | r
    )

type ChiffyE3 r
  = ( chiffyE3Gain :: TGain /\ { chiffyE3Pan :: Unit }
    , chiffyE3Pan :: TStereoPanner /\ { chiffyE3HPF :: Unit }
    , chiffyE3HPF :: THighpass /\ { chiffyE3Buf :: Unit }
    , chiffyE3Buf :: TLoopBuf /\ {}
    | r
    )

type DigistensuE3 r
  = ( digistensuE3Gain :: TGain /\ { digistensuE3Pan :: Unit }
    , digistensuE3Pan :: TStereoPanner /\ { digistensuE3HPF :: Unit }
    , digistensuE3HPF :: THighpass /\ { digistensuE3Buf :: Unit }
    , digistensuE3Buf :: TLoopBuf /\ {}
    | r
    )

type JarbleIctusE3 r
  = ( jarbleIctusE3Gain :: TGain /\ { jarbleIctusE3Pan :: Unit }
    , jarbleIctusE3Pan :: TStereoPanner /\ { jarbleIctusE3HPF :: Unit }
    , jarbleIctusE3HPF :: THighpass /\ { jarbleIctusE3Buf :: Unit }
    , jarbleIctusE3Buf :: TLoopBuf /\ {}
    | r
    )

type ShamisenE3 r
  = ( shamisenE3Gain :: TGain /\ { shamisenE3Pan :: Unit }
    , shamisenE3Pan :: TStereoPanner /\ { shamisenE3HPF :: Unit }
    , shamisenE3HPF :: THighpass /\ { shamisenE3Buf :: Unit }
    , shamisenE3Buf :: TLoopBuf /\ {}
    | r
    )

type TinefullE0 r
  = ( tinefullE0Gain :: TGain /\ { tinefullE0Pan :: Unit }
    , tinefullE0Pan :: TStereoPanner /\ { tinefullE0HPF :: Unit }
    , tinefullE0HPF :: THighpass /\ { tinefullE0Buf :: Unit }
    , tinefullE0Buf :: TLoopBuf /\ {}
    | r
    )

type TinefullE4 r
  = ( tinefullE4Gain :: TGain /\ { tinefullE4Pan :: Unit }
    , tinefullE4Pan :: TStereoPanner /\ { tinefullE4HPF :: Unit }
    , tinefullE4HPF :: THighpass /\ { tinefullE4Buf :: Unit }
    , tinefullE4Buf :: TLoopBuf /\ {}
    | r
    )

type VsMagicE3 r
  = ( vsMagicE3Gain :: TGain /\ { vsMagicE3Pan :: Unit }
    , vsMagicE3Pan :: TStereoPanner /\ { vsMagicE3HPF :: Unit }
    , vsMagicE3HPF :: THighpass /\ { vsMagicE3Buf :: Unit }
    , vsMagicE3Buf :: TLoopBuf /\ {}
    | r
    )

type ChiffyE4 r
  = ( chiffyE4Gain :: TGain /\ { chiffyE4Pan :: Unit }
    , chiffyE4Pan :: TStereoPanner /\ { chiffyE4HPF :: Unit }
    , chiffyE4HPF :: THighpass /\ { chiffyE4Buf :: Unit }
    , chiffyE4Buf :: TLoopBuf /\ {}
    | r
    )

type DigistensuE4 r
  = ( digistensuE4Gain :: TGain /\ { digistensuE4Pan :: Unit }
    , digistensuE4Pan :: TStereoPanner /\ { digistensuE4HPF :: Unit }
    , digistensuE4HPF :: THighpass /\ { digistensuE4Buf :: Unit }
    , digistensuE4Buf :: TLoopBuf /\ {}
    | r
    )

type MtDillJabsE3 r
  = ( mtDillJabsE3Gain :: TGain /\ { mtDillJabsE3Pan :: Unit }
    , mtDillJabsE3Pan :: TStereoPanner /\ { mtDillJabsE3HPF :: Unit }
    , mtDillJabsE3HPF :: THighpass /\ { mtDillJabsE3Buf :: Unit }
    , mtDillJabsE3Buf :: TLoopBuf /\ {}
    | r
    )

type ShamisenE4 r
  = ( shamisenE4Gain :: TGain /\ { shamisenE4Pan :: Unit }
    , shamisenE4Pan :: TStereoPanner /\ { shamisenE4HPF :: Unit }
    , shamisenE4HPF :: THighpass /\ { shamisenE4Buf :: Unit }
    , shamisenE4Buf :: TLoopBuf /\ {}
    | r
    )

type TinefullE1 r
  = ( tinefullE1Gain :: TGain /\ { tinefullE1Pan :: Unit }
    , tinefullE1Pan :: TStereoPanner /\ { tinefullE1HPF :: Unit }
    , tinefullE1HPF :: THighpass /\ { tinefullE1Buf :: Unit }
    , tinefullE1Buf :: TLoopBuf /\ {}
    | r
    )

type TinefullE5 r
  = ( tinefullE5Gain :: TGain /\ { tinefullE5Pan :: Unit }
    , tinefullE5Pan :: TStereoPanner /\ { tinefullE5HPF :: Unit }
    , tinefullE5HPF :: THighpass /\ { tinefullE5Buf :: Unit }
    , tinefullE5Buf :: TLoopBuf /\ {}
    | r
    )

type VsMagicE4 r
  = ( vsMagicE4Gain :: TGain /\ { vsMagicE4Pan :: Unit }
    , vsMagicE4Pan :: TStereoPanner /\ { vsMagicE4HPF :: Unit }
    , vsMagicE4HPF :: THighpass /\ { vsMagicE4Buf :: Unit }
    , vsMagicE4Buf :: TLoopBuf /\ {}
    | r
    )

type AllBuffers r
  = ChiffyE1 + ChiffyE5 + DigistensuE5 + MtDillJabsE4 + ShamisenE5 + TinefullE2 + VsMagicE1 + VsMagicE5 + ChiffyE2 + DigistensuE2 + DigistensuE6 + MtDillJabsE5 + ShamisenE6 + TinefullE3 + VsMagicE2 + ChiffyE3 + DigistensuE3 + JarbleIctusE3 + ShamisenE3 + TinefullE0 + TinefullE4 + VsMagicE3 + ChiffyE4 + DigistensuE4 + MtDillJabsE3 + ShamisenE4 + TinefullE1 + TinefullE5 + VsMagicE4 + r

type GainBus
  = { chiffyE1Gain :: Unit, chiffyE5Gain :: Unit, digistensuE5Gain :: Unit, mtDillJabsE4Gain :: Unit, shamisenE5Gain :: Unit, tinefullE2Gain :: Unit, vsMagicE1Gain :: Unit, vsMagicE5Gain :: Unit, chiffyE2Gain :: Unit, digistensuE2Gain :: Unit, digistensuE6Gain :: Unit, mtDillJabsE5Gain :: Unit, shamisenE6Gain :: Unit, tinefullE3Gain :: Unit, vsMagicE2Gain :: Unit, chiffyE3Gain :: Unit, digistensuE3Gain :: Unit, jarbleIctusE3Gain :: Unit, shamisenE3Gain :: Unit, tinefullE0Gain :: Unit, tinefullE4Gain :: Unit, vsMagicE3Gain :: Unit, chiffyE4Gain :: Unit, digistensuE4Gain :: Unit, mtDillJabsE3Gain :: Unit, shamisenE4Gain :: Unit, tinefullE1Gain :: Unit, tinefullE5Gain :: Unit, vsMagicE4Gain :: Unit }

type PreFirstVideoGraph
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ GainBus
    | AllBuffers ()
    }


preFirstVideoCreate :: forall proof. FrameSig PreFirstVideoGraph proof {} Unit
preFirstVideoCreate = create $ speaker
    { mix:
        gain 1.0
          { chiffyE1Gain: gain 0.0 { chiffyE1Pan: pan 0.0 { chiffyE1HPF: highpass 20.0 { chiffyE1Buf: loopBuf { playbackRate: 1.0 } "chiffyE1" } } }
          , chiffyE5Gain: gain 0.0 { chiffyE5Pan: pan 0.0 { chiffyE5HPF: highpass 20.0 { chiffyE5Buf: loopBuf { playbackRate: 1.0 } "chiffyE5" } } }
          , digistensuE5Gain: gain 0.0 { digistensuE5Pan: pan 0.0 { digistensuE5HPF: highpass 20.0 { digistensuE5Buf: loopBuf { playbackRate: 1.0 } "digistensuE5" } } }
          , mtDillJabsE4Gain: gain 0.0 { mtDillJabsE4Pan: pan 0.0 { mtDillJabsE4HPF: highpass 20.0 { mtDillJabsE4Buf: loopBuf { playbackRate: 1.0 } "mtDillJabsE4" } } }
          , shamisenE5Gain: gain 0.0 { shamisenE5Pan: pan 0.0 { shamisenE5HPF: highpass 20.0 { shamisenE5Buf: loopBuf { playbackRate: 1.0 } "shamisenE5" } } }
          , tinefullE2Gain: gain 0.0 { tinefullE2Pan: pan 0.0 { tinefullE2HPF: highpass 20.0 { tinefullE2Buf: loopBuf { playbackRate: 1.0 } "tinefullE2" } } }
          , vsMagicE1Gain: gain 0.0 { vsMagicE1Pan: pan 0.0 { vsMagicE1HPF: highpass 20.0 { vsMagicE1Buf: loopBuf { playbackRate: 1.0 } "vsMagicE1" } } }
          , vsMagicE5Gain: gain 0.0 { vsMagicE5Pan: pan 0.0 { vsMagicE5HPF: highpass 20.0 { vsMagicE5Buf: loopBuf { playbackRate: 1.0 } "vsMagicE5" } } }
          , chiffyE2Gain: gain 0.0 { chiffyE2Pan: pan 0.0 { chiffyE2HPF: highpass 20.0 { chiffyE2Buf: loopBuf { playbackRate: 1.0 } "chiffyE2" } } }
          , digistensuE2Gain: gain 0.0 { digistensuE2Pan: pan 0.0 { digistensuE2HPF: highpass 20.0 { digistensuE2Buf: loopBuf { playbackRate: 1.0 } "digistensuE2" } } }
          , digistensuE6Gain: gain 0.0 { digistensuE6Pan: pan 0.0 { digistensuE6HPF: highpass 20.0 { digistensuE6Buf: loopBuf { playbackRate: 1.0 } "digistensuE6" } } }
          , mtDillJabsE5Gain: gain 0.0 { mtDillJabsE5Pan: pan 0.0 { mtDillJabsE5HPF: highpass 20.0 { mtDillJabsE5Buf: loopBuf { playbackRate: 1.0 } "mtDillJabsE5" } } }
          , shamisenE6Gain: gain 0.0 { shamisenE6Pan: pan 0.0 { shamisenE6HPF: highpass 20.0 { shamisenE6Buf: loopBuf { playbackRate: 1.0 } "shamisenE6" } } }
          , tinefullE3Gain: gain 0.0 { tinefullE3Pan: pan 0.0 { tinefullE3HPF: highpass 20.0 { tinefullE3Buf: loopBuf { playbackRate: 1.0 } "tinefullE3" } } }
          , vsMagicE2Gain: gain 0.0 { vsMagicE2Pan: pan 0.0 { vsMagicE2HPF: highpass 20.0 { vsMagicE2Buf: loopBuf { playbackRate: 1.0 } "vsMagicE2" } } }
          , chiffyE3Gain: gain 0.0 { chiffyE3Pan: pan 0.0 { chiffyE3HPF: highpass 20.0 { chiffyE3Buf: loopBuf { playbackRate: 1.0 } "chiffyE3" } } }
          , digistensuE3Gain: gain 0.0 { digistensuE3Pan: pan 0.0 { digistensuE3HPF: highpass 20.0 { digistensuE3Buf: loopBuf { playbackRate: 1.0 } "digistensuE3" } } }
          , jarbleIctusE3Gain: gain 0.0 { jarbleIctusE3Pan: pan 0.0 { jarbleIctusE3HPF: highpass 20.0 { jarbleIctusE3Buf: loopBuf { playbackRate: 1.0 } "jarbleIctusE3" } } }
          , shamisenE3Gain: gain 0.0 { shamisenE3Pan: pan 0.0 { shamisenE3HPF: highpass 20.0 { shamisenE3Buf: loopBuf { playbackRate: 1.0 } "shamisenE3" } } }
          , tinefullE0Gain: gain 0.0 { tinefullE0Pan: pan 0.0 { tinefullE0HPF: highpass 20.0 { tinefullE0Buf: loopBuf { playbackRate: 1.0 } "tinefullE0" } } }
          , tinefullE4Gain: gain 0.0 { tinefullE4Pan: pan 0.0 { tinefullE4HPF: highpass 20.0 { tinefullE4Buf: loopBuf { playbackRate: 1.0 } "tinefullE4" } } }
          , vsMagicE3Gain: gain 0.0 { vsMagicE3Pan: pan 0.0 { vsMagicE3HPF: highpass 20.0 { vsMagicE3Buf: loopBuf { playbackRate: 1.0 } "vsMagicE3" } } }
          , chiffyE4Gain: gain 0.0 { chiffyE4Pan: pan 0.0 { chiffyE4HPF: highpass 20.0 { chiffyE4Buf: loopBuf { playbackRate: 1.0 } "chiffyE4" } } }
          , digistensuE4Gain: gain 0.0 { digistensuE4Pan: pan 0.0 { digistensuE4HPF: highpass 20.0 { digistensuE4Buf: loopBuf { playbackRate: 1.0 } "digistensuE4" } } }
          , mtDillJabsE3Gain: gain 0.0 { mtDillJabsE3Pan: pan 0.0 { mtDillJabsE3HPF: highpass 20.0 { mtDillJabsE3Buf: loopBuf { playbackRate: 1.0 } "mtDillJabsE3" } } }
          , shamisenE4Gain: gain 0.0 { shamisenE4Pan: pan 0.0 { shamisenE4HPF: highpass 20.0 { shamisenE4Buf: loopBuf { playbackRate: 1.0 } "shamisenE4" } } }
          , tinefullE1Gain: gain 0.0 { tinefullE1Pan: pan 0.0 { tinefullE1HPF: highpass 20.0 { tinefullE1Buf: loopBuf { playbackRate: 1.0 } "tinefullE1" } } }
          , tinefullE5Gain: gain 0.0 { tinefullE5Pan: pan 0.0 { tinefullE5HPF: highpass 20.0 { tinefullE5Buf: loopBuf { playbackRate: 1.0 } "tinefullE5" } } }
          , vsMagicE4Gain: gain 0.0 { vsMagicE4Pan: pan 0.0 { vsMagicE4HPF: highpass 20.0 { vsMagicE4Buf: loopBuf { playbackRate: 1.0 } "vsMagicE4" } } }
          }
    }
