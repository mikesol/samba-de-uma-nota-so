module SambaDeUmaNotaSo.ImageUtil where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff)
import Effect.Exception (Error)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.HTMLImageElement as HTMLImageElement
import Web.HTML.HTMLImageElement.CORSMode (CORSMode(..))

affize :: forall a. ((a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit) -> Aff a
affize f =
  makeAff \cb -> do
    f (cb <<< Right) (cb <<< Left)
    pure mempty

fetchImage :: String -> Aff HTMLImageElement.HTMLImageElement
fetchImage str =
  affize \res rej -> do
    image <- HTMLImageElement.create
    HTMLImageElement.setCrossOrigin Anonymous image
    imageListener <- eventListener \_ -> res image
    addEventListener
      (EventType "load")
      imageListener
      false
      (HTMLImageElement.toEventTarget image)
    errorListener <-
      eventListener \_ ->
        rej $ error ("Could not load image")
    addEventListener
      (EventType "error")
      errorListener
      false
      (HTMLImageElement.toEventTarget image)
    HTMLImageElement.setSrc str image
