module Main where

import Prelude
import Data.Maybe
import Control.Bind
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Var
import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe
import Data.Either.Unsafe
import Data.Foreign
import Data.Foreign.Index
import DOM.Event.EventTarget

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
    -- TODO: Figure out how to access 'data' properly, using MessageEvent
    let message = unsafeFromForeign $ fromRight $ prop "data" $ toForeign event

    log $ "onmessage: Received '" ++ message ++ "'"

    when (message == "goodbye") do
      log "onmessage: closing connection"
      socket.close Nothing Nothing

  socket.onclose $= \event -> do
    logAny event
    log "onclose: Connection closed"
