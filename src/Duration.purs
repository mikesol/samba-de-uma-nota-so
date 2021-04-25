module SambaDeUmaNotaSo.Duration where

import Prelude
import Math (ceil)
import SambaDeUmaNotaSo.Constants (beat, fourMeasures, measure)

firstVocalEnds :: Number -> Number
firstVocalEnds t = startsAtWithoutAdjustment + adjustmentIfTooClose + sectionLength
  where
  -- how many measures will have passed before we can start the section
  nMeasures = ceil (t / measure)

  -- timestamp of start without any adjustment
  startsAtWithoutAdjustment = nMeasures * measure

  -- If we are right up against a new measure, elongate by one measure
  adjustmentIfTooClose = if (t - startsAtWithoutAdjustment) < (2.0 * beat) then measure else 0.0

  -- Eis aqui este sambinha feito numa nota só
  sectionLength = fourMeasures

secondVocalEnds = firstVocalEnds :: Number -> Number

thirdVocalEnds = firstVocalEnds :: Number -> Number
