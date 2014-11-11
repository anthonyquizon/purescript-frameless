module Debug.Display.Frameless where

import Data.Maybe
import Control.Monad.Eff
import DOM
import Debug.Trace


numGrids = 40

gridOverlay :: forall eff. Eff (dom :: DOM | eff) Unit 
gridOverlay = do
    overlay <- createElement "div" >>= addClass "grid-overlay" 

    forE 0 numGrids $ \i -> do
        col <- createElement "div" 
            >>= addClass "col" 
            >>= addClass ("col-" ++ show i)
        
        col `appendChild` overlay
        return unit

    b <- body
    overlay `appendChild` b

    return unit
    
gridToggle :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit 
gridToggle = do
    Just overlay <- querySelector ".grid-overlay"
    let style = getStyle "display" overlay

    if (style == "block")
        then do
            trace "grid overlay off"
            setStyle "none" "display" overlay
        else do
            trace "grid overlay on"
            setStyle "block" "display" overlay

    return unit
    

gridEvents :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
gridEvents = do
    body >>= addEventListenerBacktickKey "keyup" gridToggle 
    return unit

addEventListenerBacktickKey :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Node -> Eff (dom :: DOM | eff) Unit
addEventListenerBacktickKey = fpi ["event", "handler", "node", ""]
    "node.addEventListener(event, function(e) {\
    \  if (e.keyCode === 192) {\
    \     handler(); \
    \  }\
    \});"
