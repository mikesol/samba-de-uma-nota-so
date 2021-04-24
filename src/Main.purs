module Main where

import Prelude
import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Compactable (compact)
import Data.Foldable (for_, traverse_)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (makeEvent, subscribe)
import FRP.Event.Mouse (Mouse, down, getMouse, withPosition)
import Foreign.Object as O
import Graphics.Canvas (CanvasElement, getContext2D)
import Graphics.Painting (ImageSources)
import Graphics.Painting as Painting
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Heterogeneous.Mapping (hmap)
import SambaDeUmaNotaSo.Action (Action(..))
import SambaDeUmaNotaSo.ClickPlayModal (clickPlay)
import SambaDeUmaNotaSo.Piece (piece)
import WAGS.Example.KitchenSink.TLP.LoopSig (SambaTrigger(..), SambaWorld)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, makeUnitCache)
import WAGS.Run (run)
import Web.HTML.HTMLElement (HTMLElement, getBoundingClientRect)

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm = let fOf initialTime = mkCofree initialTime \adj -> fOf $ max 10 (initialTime - adj) in fOf 20

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State
  = { unsubscribeFromWAGS :: Effect Unit
    , audioCtx :: Maybe AudioContext
    , mouse :: Maybe Mouse
    , world :: Maybe (Behavior SambaWorld)
    , graph :: Maybe String
    , audioSrc :: Maybe String
    }

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Ilz
              }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribeFromWAGS: pure unit
  , audioCtx: Nothing
  , graph: Nothing
  , audioSrc: Nothing
  , mouse: Nothing
  , world: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render { audioCtx } =
  HH.div [ HP.classes $ map ClassName [ "h-screen", "w-screen" ] ]
    [ HH.div
        [ HP.classes $ map ClassName [ "h-full", "w-full", "flex", "flex-col" ] ]
        [ HH.div [ HP.classes $ map ClassName [ "flex-grow" ] ]
            [ HH.canvas
                [ HP.ref (H.RefLabel "myCanvas")
                , HP.classes $ map ClassName [ "h-full", "w-full" ]
                ]
            ]
        ]
    , clickPlay { open: isNothing audioCtx }
    ]

imageSources :: ImageSources
imageSources =
  { canvases: O.empty
  , images: O.empty
  , videos: O.empty
  , webcam: Nil
  }

foreign import asCanvasElement :: HTMLElement -> (CanvasElement -> Maybe CanvasElement) -> (Maybe CanvasElement) -> Effect (Maybe CanvasElement)

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Ilz -> do
    H.getHTMLElementRef (H.RefLabel "myCanvas")
      >>= traverse_ \element -> do
          H.modify_
            _
              { world =
                Just
                  $ behavior \e ->
                      makeEvent \k -> do
                        subscribe e \aToB -> do
                          canvas <- getBoundingClientRect element
                          k $ aToB $ { canvas }
              }
    { mouse } <-
      H.liftEffect do
        mouse <- getMouse
        pure { mouse }
    H.modify_ _ { mouse = Just mouse }
  StartAudio -> do
    handleAction StopAudio
    { world, mouse } <- H.get
    H.getHTMLElementRef (H.RefLabel "myCanvas")
      >>= traverse_ \element ->
          for_ world \world' ->
            for_ mouse \mouse' -> do
              { unsubscribeFromWAGS, audioCtx } <-
                H.liftEffect do
                  let
                    mouseEvent =
                      map (Interaction <<< { touch: _ })
                        ( map (hmap toNumber)
                            (compact (map _.pos $ withPosition mouse' down))
                        )
                  audioCtx <- H.liftEffect context
                  unitCache <- H.liftEffect makeUnitCache
                  let
                    ffiAudio = defaultFFIAudio audioCtx unitCache
                  unsubscribeFromWAGS <-
                    subscribe
                      ( run
                          (mouseEvent <|> pure Start)
                          world'
                          { easingAlgorithm }
                          (FFIAudio ffiAudio)
                          piece
                      )
                      ( \{ res } -> do
                          asCanvas <- asCanvasElement element Just Nothing
                          for_ asCanvas \asCanvas' -> do
                            ctx2d <- getContext2D asCanvas'
                            Painting.render ctx2d imageSources res.painting
                      )
                  pure { unsubscribeFromWAGS, audioCtx }
              H.modify_
                _
                  { unsubscribeFromWAGS = unsubscribeFromWAGS
                  , audioCtx = Just audioCtx
                  }
  StopAudio -> do
    { unsubscribeFromWAGS, audioCtx } <- H.get
    H.liftEffect unsubscribeFromWAGS
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribeFromWAGS = pure unit, audioCtx = Nothing }
