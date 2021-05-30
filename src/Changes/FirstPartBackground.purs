module SambaDeUmaNotaSo.FirstPartBackground where

import Data.Typelevel.Num (D5, D80)
import Data.Vec ((+>))
import Data.Vec as V

data Sounds = NoS | MainS | AltS

soundMap :: V.Vec D80 (V.Vec D5 Sounds)
soundMap = (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> NoS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> NoS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> NoS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> NoS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> AltS +> MainS +> NoS +> MainS +> V.empty) +>
    (NoS +> NoS +> MainS +> AltS +> MainS +> V.empty) +>
    (NoS +> NoS +> MainS +> AltS +> MainS +> V.empty) +>
    (NoS +> NoS +> MainS +> AltS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> AltS +> NoS +> V.empty) +>
    (NoS +> MainS +> MainS +> AltS +> NoS +> V.empty) +>
    (NoS +> MainS +> MainS +> AltS +> NoS +> V.empty) +>
    (NoS +> MainS +> MainS +> AltS +> NoS +> V.empty) +>
    (NoS +> MainS +> MainS +> AltS +> NoS +> V.empty) +>
    (NoS +> MainS +> NoS +> AltS +> NoS +> V.empty) +>
    (NoS +> MainS +> NoS +> AltS +> NoS +> V.empty) +>
    (NoS +> MainS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> MainS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> MainS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> MainS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> AltS +> V.empty) +>
    (NoS +> NoS +> MainS +> MainS +> AltS +> V.empty) +>
    (NoS +> NoS +> NoS +> MainS +> AltS +> V.empty) +>
    (NoS +> NoS +> NoS +> MainS +> AltS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> AltS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> AltS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> AltS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> AltS +> V.empty) +>
    (MainS +> NoS +> MainS +> NoS +> AltS +> V.empty) +>
    (MainS +> NoS +> MainS +> NoS +> AltS +> V.empty) +>
    (MainS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (MainS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (MainS +> NoS +> MainS +> AltS +> NoS +> V.empty) +>
    (MainS +> NoS +> MainS +> AltS +> NoS +> V.empty) +>
    (MainS +> NoS +> MainS +> AltS +> NoS +> V.empty) +>
    (MainS +> NoS +> MainS +> AltS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> AltS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> AltS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> AltS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> AltS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> AltS +> NoS +> MainS +> NoS +> V.empty) +>
    (NoS +> AltS +> NoS +> MainS +> NoS +> V.empty) +>
    (NoS +> AltS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> AltS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> AltS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> AltS +> MainS +> MainS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    (NoS +> NoS +> MainS +> NoS +> NoS +> V.empty) +>
    V.empty