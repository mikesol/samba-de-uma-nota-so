module SambaDeUmaNotaSo.Types where

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Typelevel.Num (D7)
import Data.Vec (Vec)
import Graphics.Painting (Painting, Point)

type RGB
  = { r :: Int, g :: Int, b :: Int }

type Windows a
  = Vec D7 a

type Interactions
  = List Point

type BaseEnv
  = { time :: Number
    , interactions :: Interactions
    , canvas :: { w :: Number, h :: Number }
    }

type AugmentedEnv
  = { time :: Number
    , interactions :: Interactions
    , canvas :: { w :: Number, h :: Number }
    , background :: Painting
    }

type FirstPartEnv
  = { time :: Number
    , interactions :: Interactions
    , canvas :: { w :: Number, h :: Number }
    , background :: Painting
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , isWindowTouched :: Windows Boolean
    }

type VideoSpan
  = { start :: Number, duration :: Number }

{-
type WithWindowOnScreen' r
  = ( windowOnScreen :: Window -> Painting | r )

type WithWindowAndVideoOnScreen' r
  = ( windowAndVideoOnScreen :: Window -> Painting | r )

type AddWindowOnScreen' r
  = WithCanvas' + WithWindowInteractions' + WithTime' + r

type AddWindowAndVideoOnScreen' r
  = WithWindowOnScreen' + WithCanvas' + WithTime' + r

type WithWindow' r
  = ( window :: Window | r )

type WithVideoSpan' r
  = ( videoSpan :: { start :: Number, duration :: Number } | r )

type VideoPlayingInfo' r
  = (WithWindow' + WithVideoSpan' + WithPrevInter' + r)



type OnsetList
  = List { onset :: Number }

type WithPrevInter' r
  = ( interCount :: Int | r )

type WithNSincePrevInter' r
  = ( nSincePrevInter :: Int | r )

type WithMemoizedWindowInteractions' r
  = ( windowInteractions :: Window' OnsetList | r )

type InfoForFirstPartEnv r
  = WithMemoizedWindowInteractions' (WithPrevInter' r)

type RPreFirstVideoInfo
  = { | InfoForFirstPartEnv () }

type RAwaitingFirstVideoInfo
  = { | InfoForFirstPartEnv (WithWindow' ()) }

type RFirstVideoInfo
  = { | InfoForFirstPartEnv (VideoPlayingInfo' ()) }

type RPreSecondVideoInfo
  = { | InfoForFirstPartEnv (WithNSincePrevInter' ()) }
-}
