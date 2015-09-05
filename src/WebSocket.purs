-- | This module defines a simple low-level interface to the websockets API.

module WebSocket
  ( WebSocket()
  , Socket()
  , URI()
  , Message()
  , mkWebSocket
  , onMessage
  , onError
  , onOpen
  , onClose
  , send
  ) where

import Prelude
import Control.Monad.Eff
import Data.Function

-- | The effect associated with websocket connections.
foreign import data WebSocket :: !

-- | A reference to a websocket.
foreign import data Socket :: *

-- | A synonym for URI strings.
type URI = String

-- | A synonym for message strings.
type Message = String

-- | Create a websocket object for a URI.
foreign import mkWebSocket :: forall e. URI -> Eff (ws :: WebSocket | e) Socket

foreign import onMessageImpl :: forall e a.
                Fn2 Socket
                    (Message -> Eff (ws :: WebSocket | e) a)
                    (Eff (ws :: WebSocket | e) Unit)

-- | Register a callback for incoming messages.
onMessage :: forall e a.
             Socket -> (Message -> Eff (ws :: WebSocket | e) a)
                    -> Eff (ws :: WebSocket | e) Unit
onMessage socket callback = runFn2 onMessageImpl socket callback

foreign import onErrorImpl :: forall e a.
                Fn2 Socket
                    (Eff (ws :: WebSocket | e) a)
                    (Eff (ws :: WebSocket | e) Unit)

-- | Register a callback for `error` events.
onError :: forall e a.
            Socket -> Eff (ws :: WebSocket | e) a
                   -> Eff (ws :: WebSocket | e) Unit
onError socket callback = runFn2 onErrorImpl socket callback

foreign import onOpenImpl :: forall e a.
                Fn2 Socket
                    (Eff (ws :: WebSocket | e) a)
                    (Eff (ws :: WebSocket | e) Unit)

-- | Register a callback for `open` events.
onOpen :: forall e a.
            Socket -> Eff (ws :: WebSocket | e) a
                   -> Eff (ws :: WebSocket | e) Unit
onOpen socket callback = runFn2 onOpenImpl socket callback

foreign import onCloseImpl :: forall e a.
                Fn2 Socket
                    (Eff (ws :: WebSocket | e) a)
                    (Eff (ws :: WebSocket | e) Unit)

-- | Register a callback for `close` events.
onClose :: forall e a.
            Socket -> Eff (ws :: WebSocket | e) a
                   -> Eff (ws :: WebSocket | e) Unit

onClose socket callback = runFn2 onCloseImpl socket callback

foreign import sendImpl :: forall e.
                Fn2 Socket Message (Eff (ws :: WebSocket | e) Unit)

-- | Send a message to a websocket.
send :: forall e.
         Socket -> Message -> Eff (ws :: WebSocket | e) Unit
send socket msg  = runFn2 sendImpl socket msg
