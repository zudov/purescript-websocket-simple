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
  Connection socket <- newWebSocket "ws://echo.websocket.org" []

  socket.onopen $= \event -> do
    logAny event
    log "onopen: Connection opened"

    log =<< get socket.url

    log "onopen: Sending 'hello'"
    socket.send "hello"

    log "onopen: Sending 'goodbye'"
    socket.send "goodbye"

  socket.onmessage $= \event -> do
    logAny event
    let message = messageData event

    log $ "onmessage: Received '" ++ messageData event ++ "'"

    when (message == "goodbye") do
      log "onmessage: closing connection"
      socket.close Nothing Nothing

  socket.onclose $= \event -> do
    logAny event
    log "onclose: Connection closed"
