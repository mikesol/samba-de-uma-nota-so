module SambaDeUmaNotaSo.Constants where

import Prelude

bpm = 160.0 :: Number

beat = 60.0 / bpm :: Number

beatsInMeasure = 4.0 :: Number

measure = beat * beatsInMeasure :: Number

fourMeasures = measure * 4.0 :: Number

oneBeat = beat * 1.0 :: Number
twoBeats = beat * 2.0 :: Number
threeBeats = beat * 3.0 :: Number
fourBeats = beat * 4.0 :: Number
fiveBeats = beat * 5.0 :: Number
sixBeats = beat * 6.0 :: Number
sevenBeats = beat * 7.0 :: Number

-- | How long a window should last when clicked.
-- | For now this is 4 beats, but it could be shorter/longer.
beatsInWindow = 4.0 :: Number

windowLength = beat * beatsInWindow :: Number

-- for drawing video boxes
start = 0.0 :: Number

ptTop0 = 0.25 :: Number

ptTop1 = 0.7 :: Number

ptLeft0 = 0.45 :: Number

ptLeft1 = 0.75 :: Number

ptBottom0 = 0.33 :: Number

ptRight0 = 0.25 :: Number

end = 1.0 :: Number

-- makes mod operations feel more random because they're more sensitive
-- to the current timestamp
jitterForMod = 10000.0 :: Number
