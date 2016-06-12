module Main where

import Prelude

import Control.Bind ((=<<))
import Control.Monad (when)
import Control.Monad.Eff.Var (($=), get)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Console.Unsafe (logAny)
import Data.Maybe (Maybe(..))

import WebSocket

main = do
  Connection socket <- newWebSocket (URL "ws://echo.websocket.org") []

  socket.onopen $= \event -> do
    logAny event
    log "onopen: Connection opened"

    log <<< runURL =<< get socket.url

    log "onopen: Sending 'hello'"
    socket.send (Message "hello")

    log "onopen: Sending 'goodbye'"
    socket.send (Message "goodbye")

  socket.onmessage $= \event -> do
    logAny event
    let received = runMessage (runMessageEvent event)

    log $ "onmessage: Received '" ++ received ++ "'"

    when (received == "goodbye") do
      log "onmessage: closing connection"
      socket.close

  socket.onclose $= \event -> do
    logAny event
    log "onclose: Connection closed"
