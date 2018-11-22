module Main where

import Prelude

import Effect (Effect)
import Effect.Var (($=), get)
import Effect.Console (log)
import Debug.Trace (traceM, class DebugWarning)

import WebSocket (Connection(..), Message(..), URL(..), runMessageEvent, runMessage, runURL, newWebSocket)

main :: Effect Unit
main = do
  Connection socket <- newWebSocket (URL "ws://echo.websocket.org") []

  socket.onopen $= \event -> do
    traceM event
    log "onopen: Connection opened"

    log <<< runURL =<< get socket.url

    log "onopen: Sending 'hello'"
    socket.send (Message "hello")

    log "onopen: Sending 'goodbye'"
    socket.send (Message "goodbye")

  socket.onmessage $= \event -> do
    traceM event
    let received = runMessage (runMessageEvent event)

    log $ "onmessage: Received '" <> received <> "'"

    when (received == "goodbye") do
      log "onmessage: closing connection"
      socket.close

  socket.onclose $= \event -> do
    traceM event
    log "onclose: Connection closed"
