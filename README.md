# Simple Bindings to Websocket API for Purescript

## Example usage

```haskell
module Main where

import Prelude
import Control.Monad.Eff.Console
import WebSocket

main = do
  ws <- mkWebSocket "ws://echo.websocket.org"
  onMessage ws log
  onOpen ws $ do
    send ws "hello"
    send ws "world"
```

