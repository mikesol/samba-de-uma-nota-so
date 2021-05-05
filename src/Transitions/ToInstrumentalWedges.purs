module SambaDeUmaNotaSo.ToInstrumentalWedges where

import Prelude
import Color (rgb)
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Graphics.Painting (Painting, arc, fillColor, filled, translate, withMove)
import Math (pi)
import SambaDeUmaNotaSo.Constants (beats)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

instrumentalAnimation :: Number -> NonEmptyToCofree DOMRect Painting
instrumentalAnimation startsAt =
  nonEmptyToCofree (Just wedge13)
    ( (pos (beats 16.0) /\ (const mempty))
        :| ( (pos (beats 17.0) /\ wedge0)
              : (pos (beats 17.5) /\ wedge1)
              : (pos (beats 18.5) /\ wedge2)
              : (pos (beats 19.5) /\ wedge3)
              : (pos (beats 20.5) /\ wedge4)
              : (pos (beats 21.5) /\ wedge5)
              : (pos (beats 22.0) /\ wedge6)
              : (pos (beats 23.0) /\ wedge7)
              : (pos (beats 24.0) /\ wedge8)
              : (pos (beats 25.0) /\ wedge9)
              : (pos (beats 25.5) /\ wedge10)
              : (pos (beats 26.5) /\ wedge11)
              : (pos (beats 27.5) /\ wedge12)
              : (pos (beats 28.5) /\ wedge13)
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

-- 0 2 3 5 7 9 11 12 14 16 18 19 21 23
wedge0 :: DOMRect -> Painting
wedge0 { width, height } = (translate (width * -0.199) (height * 0.002) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.590 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge1 :: DOMRect -> Painting
wedge1 { width, height } = (translate (width * -0.157) (height * 0.001) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.678 * mwh * 0.45)))) <> (translate (width * 0.165) (height * 0.196) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.599 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge2 :: DOMRect -> Painting
wedge2 { width, height } = (translate (width * -0.129) (height * 0.001) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.735 * mwh * 0.45)))) <> (translate (width * 0.129) (height * 0.152) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.688 * mwh * 0.45)))) <> (translate (width * 0.102) (height * -0.120) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.608 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge3 :: DOMRect -> Painting
wedge3 { width, height } = (translate (width * -0.108) (height * 0.001) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.778 * mwh * 0.45)))) <> (translate (width * 0.105) (height * 0.124) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.746 * mwh * 0.45)))) <> (translate (width * 0.079) (height * -0.092) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.699 * mwh * 0.45)))) <> (translate (width * -0.115) (height * -0.093) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.619 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge4 :: DOMRect -> Painting
wedge4 { width, height } = (translate (width * -0.091) (height * 0.001) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.814 * mwh * 0.45)))) <> (translate (width * 0.086) (height * 0.102) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.790 * mwh * 0.45)))) <> (translate (width * 0.063) (height * -0.074) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.758 * mwh * 0.45)))) <> (translate (width * -0.087) (height * -0.070) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.711 * mwh * 0.45)))) <> (translate (width * 0.070) (height * -0.008) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (0.631 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge5 :: DOMRect -> Painting
wedge5 { width, height } = (translate (width * -0.076) (height * 0.001) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.844 * mwh * 0.45)))) <> (translate (width * 0.072) (height * 0.085) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.826 * mwh * 0.45)))) <> (translate (width * 0.052) (height * -0.061) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.803 * mwh * 0.45)))) <> (translate (width * -0.069) (height * -0.056) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.771 * mwh * 0.45)))) <> (translate (width * 0.052) (height * -0.006) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (0.725 * mwh * 0.45)))) <> (translate (width * -0.012) (height * 0.050) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (0.644 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge6 :: DOMRect -> Painting
wedge6 { width, height } = (translate (width * -0.063) (height * 0.001) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.871 * mwh * 0.45)))) <> (translate (width * 0.059) (height * 0.070) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.857 * mwh * 0.45)))) <> (translate (width * 0.042) (height * -0.049) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.839 * mwh * 0.45)))) <> (translate (width * -0.055) (height * -0.045) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.817 * mwh * 0.45)))) <> (translate (width * 0.041) (height * -0.005) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (0.786 * mwh * 0.45)))) <> (translate (width * -0.009) (height * 0.036) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (0.740 * mwh * 0.45)))) <> (translate (width * 0.132) (height * 0.155) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.5) (pi * 0.6666666666666666) (0.660 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge7 :: DOMRect -> Painting
wedge7 { width, height } = (translate (width * -0.052) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.894 * mwh * 0.45)))) <> (translate (width * 0.048) (height * 0.057) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.884 * mwh * 0.45)))) <> (translate (width * 0.034) (height * -0.040) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.871 * mwh * 0.45)))) <> (translate (width * -0.044) (height * -0.035) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.854 * mwh * 0.45)))) <> (translate (width * 0.032) (height * -0.004) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (0.833 * mwh * 0.45)))) <> (translate (width * -0.007) (height * 0.028) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (0.803 * mwh * 0.45)))) <> (translate (width * 0.094) (height * 0.110) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.5) (pi * 0.6666666666666666) (0.758 * mwh * 0.45)))) <> (translate (width * 0.061) (height * -0.109) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * -0.16666666666666666) (pi * 0.0) (0.678 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge8 :: DOMRect -> Painting
wedge8 { width, height } = (translate (width * -0.041) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.915 * mwh * 0.45)))) <> (translate (width * 0.038) (height * 0.045) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.907 * mwh * 0.45)))) <> (translate (width * 0.027) (height * -0.031) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.898 * mwh * 0.45)))) <> (translate (width * -0.035) (height * -0.028) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.886 * mwh * 0.45)))) <> (translate (width * 0.025) (height * -0.003) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (0.871 * mwh * 0.45)))) <> (translate (width * -0.005) (height * 0.021) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (0.850 * mwh * 0.45)))) <> (translate (width * 0.069) (height * 0.081) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.5) (pi * 0.6666666666666666) (0.822 * mwh * 0.45)))) <> (translate (width * 0.042) (height * -0.075) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * -0.16666666666666666) (pi * 0.0) (0.778 * mwh * 0.45)))) <> (translate (width * -0.103) (height * -0.047) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.16666666666666666) (pi * 0.3333333333333333) (0.699 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge9 :: DOMRect -> Painting
wedge9 { width, height } = (translate (width * -0.032) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.935 * mwh * 0.45)))) <> (translate (width * 0.029) (height * 0.035) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.929 * mwh * 0.45)))) <> (translate (width * 0.020) (height * -0.024) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.922 * mwh * 0.45)))) <> (translate (width * -0.026) (height * -0.021) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.914 * mwh * 0.45)))) <> (translate (width * 0.018) (height * -0.002) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (0.903 * mwh * 0.45)))) <> (translate (width * -0.004) (height * 0.015) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (0.889 * mwh * 0.45)))) <> (translate (width * 0.050) (height * 0.059) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.5) (pi * 0.6666666666666666) (0.871 * mwh * 0.45)))) <> (translate (width * 0.029) (height * -0.053) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * -0.16666666666666666) (pi * 0.0) (0.844 * mwh * 0.45)))) <> (translate (width * -0.068) (height * -0.031) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.16666666666666666) (pi * 0.3333333333333333) (0.803 * mwh * 0.45)))) <> (translate (width * -0.020) (height * 0.080) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.0) (pi * 1.1666666666666667) (0.725 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge10 :: DOMRect -> Painting
wedge10 { width, height } = (translate (width * -0.023) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.953 * mwh * 0.45)))) <> (translate (width * 0.021) (height * 0.025) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.949 * mwh * 0.45)))) <> (translate (width * 0.015) (height * -0.017) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.944 * mwh * 0.45)))) <> (translate (width * -0.019) (height * -0.015) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.938 * mwh * 0.45)))) <> (translate (width * 0.013) (height * -0.002) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (0.931 * mwh * 0.45)))) <> (translate (width * -0.003) (height * 0.011) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (0.922 * mwh * 0.45)))) <> (translate (width * 0.035) (height * 0.041) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.5) (pi * 0.6666666666666666) (0.910 * mwh * 0.45)))) <> (translate (width * 0.020) (height * -0.036) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * -0.16666666666666666) (pi * 0.0) (0.894 * mwh * 0.45)))) <> (translate (width * -0.044) (height * -0.020) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.16666666666666666) (pi * 0.3333333333333333) (0.871 * mwh * 0.45)))) <> (translate (width * -0.012) (height * 0.049) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.0) (pi * 1.1666666666666667) (0.833 * mwh * 0.45)))) <> (translate (width * 0.023) (height * 0.019) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.6666666666666666) (pi * 0.8333333333333334) (0.758 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge11 :: DOMRect -> Painting
wedge11 { width, height } = (translate (width * -0.015) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.970 * mwh * 0.45)))) <> (translate (width * 0.014) (height * 0.016) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.967 * mwh * 0.45)))) <> (translate (width * 0.009) (height * -0.011) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.964 * mwh * 0.45)))) <> (translate (width * -0.012) (height * -0.010) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.961 * mwh * 0.45)))) <> (translate (width * 0.008) (height * -0.001) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (0.956 * mwh * 0.45)))) <> (translate (width * -0.002) (height * 0.007) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (0.951 * mwh * 0.45)))) <> (translate (width * 0.022) (height * 0.025) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.5) (pi * 0.6666666666666666) (0.944 * mwh * 0.45)))) <> (translate (width * 0.012) (height * -0.022) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * -0.16666666666666666) (pi * 0.0) (0.935 * mwh * 0.45)))) <> (translate (width * -0.027) (height * -0.012) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.16666666666666666) (pi * 0.3333333333333333) (0.922 * mwh * 0.45)))) <> (translate (width * -0.007) (height * 0.028) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.0) (pi * 1.1666666666666667) (0.903 * mwh * 0.45)))) <> (translate (width * 0.013) (height * 0.010) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.6666666666666666) (pi * 0.8333333333333334) (0.871 * mwh * 0.45)))) <> (translate (width * -0.082) (height * -0.075) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.6666666666666667) (pi * 1.8333333333333333) (0.803 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge12 :: DOMRect -> Painting
wedge12 { width, height } = (translate (width * -0.007) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (0.985 * mwh * 0.45)))) <> (translate (width * 0.007) (height * 0.008) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (0.984 * mwh * 0.45)))) <> (translate (width * 0.005) (height * -0.005) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (0.983 * mwh * 0.45)))) <> (translate (width * -0.006) (height * -0.005) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (0.981 * mwh * 0.45)))) <> (translate (width * 0.004) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (0.979 * mwh * 0.45)))) <> (translate (width * -0.001) (height * 0.003) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (0.977 * mwh * 0.45)))) <> (translate (width * 0.010) (height * 0.012) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.5) (pi * 0.6666666666666666) (0.974 * mwh * 0.45)))) <> (translate (width * 0.006) (height * -0.010) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * -0.16666666666666666) (pi * 0.0) (0.970 * mwh * 0.45)))) <> (translate (width * -0.012) (height * -0.006) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.16666666666666666) (pi * 0.3333333333333333) (0.964 * mwh * 0.45)))) <> (translate (width * -0.003) (height * 0.013) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.0) (pi * 1.1666666666666667) (0.956 * mwh * 0.45)))) <> (translate (width * 0.005) (height * 0.004) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.6666666666666666) (pi * 0.8333333333333334) (0.944 * mwh * 0.45)))) <> (translate (width * -0.032) (height * -0.030) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.6666666666666667) (pi * 1.8333333333333333) (0.922 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height

wedge13 :: DOMRect -> Painting
wedge13 { width, height } = (translate (width * -0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.1666666666666667) (pi * 1.3333333333333333) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.0) (pi * 0.16666666666666666) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.5) (pi * 1.6666666666666667) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.3333333333333333) (pi * 0.5) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.8333333333333334) (pi * 1.0) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.3333333333333333) (pi * 1.5) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.5) (pi * 0.6666666666666666) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * -0.16666666666666666) (pi * 0.0) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.16666666666666666) (pi * 0.3333333333333333) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.0) (pi * 1.1666666666666667) (1.000 * mwh * 0.45)))) <> (translate (width * 0.000) (height * 0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 0.6666666666666666) (pi * 0.8333333333333334) (1.000 * mwh * 0.45)))) <> (translate (width * -0.000) (height * -0.000) $ filled (fillColor (rgb 255 255 255)) (withMove halfW halfH true (arc halfW halfH (pi * 1.6666666666666667) (pi * 1.8333333333333333) (1.000 * mwh * 0.45))))
  where
  halfW = width / 2.0

  halfH = height / 2.0

  mwh = min width height
