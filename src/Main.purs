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
import Effect.Ref as Ref
import FRP.Behavior (behavior)
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
import WAGS.Example.KitchenSink.TLP.LoopSig (SambaTrigger(..))
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, makeUnitCache)
import WAGS.Run (run)
import Web.HTML as HTML
import Web.HTML.HTMLElement (HTMLElement, getBoundingClientRect)
import Web.HTML.Window as Window

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
    { mouse } <-
      H.liftEffect do
        mouse <- getMouse
        pure { mouse }
    H.modify_ _ { mouse = Just mouse }
  StartAudio -> do
    handleAction StopAudio
    { mouse } <- H.get
    H.getHTMLElementRef (H.RefLabel "myCanvas")
      >>= traverse_ \element -> do
          let
            world =
              behavior \e ->
                makeEvent \k -> do
                  subscribe e \aToB -> do
                    canvas <- getBoundingClientRect element
                    k $ aToB $ { canvas }
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
                refId <- Ref.new Nothing
                refPainting <- Ref.new Nothing
                let
                  loop = do
                    asCanvas <- asCanvasElement element Just Nothing
                    for_ asCanvas \asCanvas' -> do
                      painting <- Ref.read refPainting
                      ctx2d <- getContext2D asCanvas'
                      for_ painting $ Painting.render ctx2d imageSources
                    w <- HTML.window
                    id <- Window.requestAnimationFrame loop w
                    Ref.write (Just id) refId
                loop
                unsub <-
                  subscribe
                    ( run
                        (mouseEvent <|> pure Start)
                        world
                        { easingAlgorithm }
                        (FFIAudio ffiAudio)
                        piece
                    )
                    (\{ res: { painting } } -> Ref.write (Just painting) refPainting)
                pure
                  { unsubscribeFromWAGS:
                      do
                        unsub
                        id <- Ref.read refId
                        w <- HTML.window
                        for_ id $ flip Window.cancelAnimationFrame w
                  , audioCtx
                  }
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
