-- | Event sources to use with Halogen, e.g.
-- | ```
-- | H.subscribe $ HES.eventSource' (onDocumentMouseUp) (Just <<< H.request <<< DocumentClick)
-- | H.subscribe $ HES.eventSource' ((ignoreInside wrapper) >>> onDocumentMouseUp) (Just <<< H.request <<< DocumentClick)
-- | ```
module App.EventSources (
  onMouseUp,
  onDocumentMouseUp,
  onHtmlElementMouseUp
  ) where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML(window)
import DOM.HTML.Window as Win
import DOM.Event.EventTarget as DEET
import DOM.Event.Types as DET
import DOM.Node.Node as DNN
import DOM.Event.Event as DEE
import DOM.HTML.Event.EventTypes as EventTypes
import DOM.HTML.Types as DHT
import Prelude (Unit, discard, ($), pure, (<<<), (>>>), (=<<), (>>=), bind, (<$>), unit)

type Callback e = (DET.Event -> Eff (dom :: DOM | e) Unit)
type RemoveListener e = Eff (dom :: DOM | e) (Eff (dom :: DOM | e) Unit)
type CallbackUnsubscribe e = Callback e -> RemoveListener e

-- | Ignore events who's target is inside a certain element
ignoreInside :: forall e. DHT.HTMLElement -> Callback e -> Callback e
ignoreInside exclude trigger =
  let wrapper  = DHT.htmlElementToNode exclude
  in (\event -> do
    isInside <- DNN.contains wrapper (DEE.target event)
    if (isInside)
      then pure unit
      else trigger event
  )

-- | Listen for mouseup events on the document
onDocumentMouseUp :: forall e. CallbackUnsubscribe e
onDocumentMouseUp cb = do
  et <- DHT.htmlDocumentToEventTarget <$> (window >>= Win.document)
  onMouseUp et cb

-- | Listen for mouseup events on an element
onHtmlElementMouseUp ::  forall e. DHT.HTMLElement -> CallbackUnsubscribe e
onHtmlElementMouseUp = onMouseUp <<< DHT.htmlElementToEventTarget

-- | Listen for mouseup events on an event target
onMouseUp :: forall e. DET.EventTarget -> CallbackUnsubscribe e
onMouseUp eventTarget callback = do
  -- Create an EventListener that will log a message
  -- and pass the event to the callback
  let listener =
        DEET.eventListener (\event -> do
          callback event
        )
  -- Add the EventListener to the document so it will fire on mouseup
  DEET.addEventListener EventTypes.mouseup listener true eventTarget

  -- Return the function that will remove the event listener
  pure $ DEET.removeEventListener EventTypes.mouseup listener true eventTarget
