module SambaDeUmaNotaSo.Constants where

import Prelude

bpm = 160.0 :: Number

beat = 60.0 / bpm :: Number

measure = beat * 4.0 :: Number

windowLength = beat * 4.0 :: Number

-- for drawing video boxes
start = 0.0 :: Number

ptTop0 = 0.25 :: Number

ptTop1 = 0.7 :: Number

ptLeft0 = 0.45 :: Number

ptLeft1 = 0.75 :: Number

ptBottom0 = 0.33 :: Number

ptRight0 = 0.25 :: Number

end = 1.0 :: Number