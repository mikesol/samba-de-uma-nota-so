module SambaDeUmaNotaSo.Constants where

import Prelude

bpm = 160.0 :: Number

beat = 60.0 / bpm :: Number

beats :: Number -> Number
beats m = beat * m

beatsInMeasure = 4.0 :: Number

measure = beat * beatsInMeasure :: Number

fourMeasures = measure * 4.0 :: Number

oneBeat = beat * 1.0 :: Number

oneAndAHalfBeats = beat * 1.5 :: Number

twoBeats = beat * 2.0 :: Number

twoAndAHalfBeats = beat * 2.5 :: Number

threeBeats = beat * 3.0 :: Number

threeAndAHalfBeats = beat * 3.5 :: Number

fourBeats = beat * 4.0 :: Number

fourAndAHalfBeats = beat * 4.5 :: Number

fiveBeats = beat * 5.0 :: Number

fiveAndAHalfBeats = beat * 5.5 :: Number

sixBeats = beat * 6.0 :: Number

sixAndAHalfBeats = beat * 6.5 :: Number

sevenBeats = beat * 7.0 :: Number

sevenAndAHalfBeats = beat * 7.5 :: Number

eightBeats = beat * 8.0 :: Number

eightAndAHalfBeats = beat * 8.5 :: Number

nineBeats = beat * 9.0 :: Number

nineAndAHalfBeats = beat * 9.5 :: Number

tenBeats = beat * 10.0 :: Number

tenAndAHalfBeats = beat * 10.5 :: Number

elevenBeats = beat * 11.0 :: Number

elevenAndAHalfBeats = beat * 11.5 :: Number

twelveBeats = beat * 12.0 :: Number

twelveAndAHalfBeats = beat * 12.5 :: Number

thirteenBeats = beat * 13.0 :: Number

thirteenAndAHalfBeats = beat * 13.5 :: Number

fourteenBeats = beat * 14.0 :: Number

fourteenAndAHalfBeats = beat * 14.5 :: Number

fifteenBeats = beat * 15.0 :: Number

fifteenAndAHalfBeats = beat * 15.5 :: Number

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
