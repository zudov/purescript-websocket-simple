-- | This module defines a simple low-level interface to the websockets API.

module WebSocket
  ( WEBSOCKET()
  , WebSocket()
  , newWebSocket
  , Connection(..)
  , URL()
  , Message()
  , messageData
  , Code(..)
  , Reason(..)
  , ReadyState(..)
  , Protocol(..)
  , BufferedAmount(..)
  , BinaryType(..)
  ) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Var
import Data.Function
import Data.Functor.Invariant
import Data.Functor.Contravariant
import Data.Nullable
import DOM.Event.EventTarget
import DOM.Event.Types
import Data.Maybe
import Data.Generic
import Data.Enum
import Unsafe.Coerce
import Data.Either
import Data.Foreign
import Data.Foreign.Index

foreign import specViolation :: forall a. String -> a

-- | The effect associated with websocket connections.
foreign import data WEBSOCKET :: !

-- | A reference to a WebSocket object.
foreign import data WebSocket :: *

-- | Initiate a websocket connection.
newWebSocket :: forall eff. URL -> Array Protocol -> Eff (ws :: WEBSOCKET | eff) Connection
newWebSocket url protocols = enhanceConnection <$> runFn2 newWebSocketImpl url protocols

foreign import newWebSocketImpl :: forall eff. Fn2 URL
                                                   (Array Protocol)
                                                   (Eff (ws :: WEBSOCKET | eff) ConnectionImpl)

messageData :: MessageEvent -> Message
messageData event = case prop "data" (toForeign event) of
                      Right x -> unsafeFromForeign x
                      Left _  -> specViolation "'data' missing from MessageEvent"

type ConnectionImpl =
  { setBinaryType     :: forall eff. String -> Eff (ws :: WEBSOCKET | eff) Unit
  , getBinaryType     :: forall eff. Eff (ws :: WEBSOCKET | eff) String
  , getBufferedAmount :: forall eff. Eff (ws :: WEBSOCKET | eff) Int
  , setOnclose        :: forall eff handlerEff. EventListener handlerEff -> Eff (ws :: WEBSOCKET | eff) Unit
  , setOnerror        :: forall eff handlerEff. EventListener handlerEff -> Eff (ws :: WEBSOCKET | eff) Unit
  , setOnmessage      :: forall eff handlerEff. EventListener handlerEff -> Eff (ws :: WEBSOCKET | eff) Unit
  , setOnopen         :: forall eff handlerEff. EventListener handlerEff -> Eff (ws :: WEBSOCKET | eff) Unit
  , setProtocol       :: forall eff. String -> Eff (ws :: WEBSOCKET | eff) Unit
  , getProtocol       :: forall eff. Eff (ws :: WEBSOCKET | eff) String
  , getReadyState     :: forall eff. Eff (ws :: WEBSOCKET | eff) Int
  , getUrl            :: forall eff. Eff (ws :: WEBSOCKET | eff) String
  , closeImpl         :: forall eff. Maybe Code -> Maybe Reason -> Eff (ws :: WEBSOCKET | eff) Unit
  , sendImpl          :: forall eff. String -> Eff (ws :: WEBSOCKET | eff) Unit
  , getSocket         :: forall eff. Eff (ws :: WEBSOCKET | eff) WebSocket
  }

coerceEvent :: forall a. Event -> a
coerceEvent = unsafeCoerce

enhanceConnection :: ConnectionImpl -> Connection
enhanceConnection c = Connection
  { binaryType: imap toBinaryType fromBinaryType $ makeVar c.getBinaryType c.setBinaryType
  , bufferedAmount: makeGettableVar c.getBufferedAmount
  , onclose: cmap (eventListener <<< (`map` coerceEvent)) (makeSettableVar c.setOnclose)
  , onerror: cmap (eventListener <<< (`map` coerceEvent)) (makeSettableVar c.setOnerror)
  , onmessage: cmap (eventListener <<< (`map` coerceEvent)) (makeSettableVar c.setOnmessage)
  , onopen: cmap (eventListener <<< (`map` coerceEvent)) (makeSettableVar c.setOnopen)
  , protocol: makeVar c.getProtocol c.setProtocol
  , readyState: unsafeReadyState <$> makeGettableVar c.getReadyState
  , url: makeGettableVar c.getUrl
  , close: c.closeImpl
  , send: c.sendImpl
  , socket: makeGettableVar c.getSocket
  }
  where
    unsafeReadyState :: Int -> ReadyState
    unsafeReadyState x =
      fromMaybe (specViolation "readyState isn't in the range of valid constants")
                (toEnum x)


-- | - `binaryType` -- The type of binary data being transmitted by the connection.
-- | - `bufferedAmount` -- The number of bytes of data that have been queued
--                         using calls to `send` but not yet transmitted to the
--                         network. This value does not reset to zero when the
--                         connection is closed; if you keep calling `send`,
--                         this will continue to climb.
-- | - `onclose` -- An event listener to be called when the `Connection`'s
-- |                `readyState` changes to `Closed`.
-- | - `onerror` -- An event listener to be called when an error occurs.
-- | - `onmessage` -- An event listener to be called when a message is received
-- |                  from the server.
-- | - `onopen` -- An event listener to be called when the `Connection`'s
-- |               readyState changes to `Open`; this indicates that the
-- |               connection is ready to send and receive data.
-- | - `protocol` -- A string indicating the name of the sub-protocol the server selected.
-- | - `readyState` -- The current state of the connection.
-- | - `url` -- The URL as resolved by during construction. This is always an absolute URL.
-- | - `close` -- Closes the connection or connection attempt, if any.
-- |              If the connection is already CLOSED, this method does nothing.
-- |              If `Code` isn't specified a default value of 1000 (indicating
-- |               a normal "transaction complete" closure) is assumed
-- | - `send` -- Transmits data to the server.
-- | - `socket` -- Reference to closured WebSocket object.
newtype Connection = Connection
  { binaryType     :: forall eff. Var (ws :: WEBSOCKET | eff) BinaryType
  , bufferedAmount :: forall eff. GettableVar (ws :: WEBSOCKET | eff) BufferedAmount
  , onclose        :: forall eff handlerEff. SettableVar (ws :: WEBSOCKET | eff) (CloseEvent -> Eff handlerEff Unit)
  , onerror        :: forall eff handlerEff. SettableVar (ws :: WEBSOCKET | eff) (Event -> Eff handlerEff Unit)
  , onmessage      :: forall eff handlerEff. SettableVar (ws :: WEBSOCKET | eff) (MessageEvent -> Eff handlerEff Unit)
  , onopen         :: forall eff handlerEff. SettableVar (ws :: WEBSOCKET | eff) (Event -> Eff handlerEff Unit)
  , protocol       :: forall eff. Var (ws :: WEBSOCKET | eff) Protocol
  , readyState     :: forall eff. GettableVar (ws :: WEBSOCKET | eff) ReadyState
  , url            :: forall eff. GettableVar (ws :: WEBSOCKET | eff) URL
  , close          :: forall eff. Maybe Code -> Maybe Reason -> Eff (ws :: WEBSOCKET | eff) Unit
  , send           :: forall eff. String -> Eff (ws :: WEBSOCKET | eff) Unit
  , socket         :: forall eff. GettableVar (ws :: WEBSOCKET | eff) WebSocket
  }

-- | The type of binary data being transmitted by the connection.
data BinaryType = Blob | ArrayBuffer

toBinaryType :: String -> BinaryType
toBinaryType "blob" = Blob
toBinaryType "arraybuffer" = ArrayBuffer
toBinaryType s = specViolation "binaryType should be either 'blob' or 'arraybuffer'"

fromBinaryType :: BinaryType -> String
fromBinaryType Blob = "blob"
fromBinaryType ArrayBuffer = "arraybuffer"

-- | The number of bytes of data that have been buffered (queued but not yet transmitted)
type BufferedAmount = Int

-- | A string indicating the name of the sub-protocol.
type Protocol = String

-- | State of the connection.
data ReadyState = Connecting | Open | Closing | Closed

derive instance genericReadyState :: Generic ReadyState

instance eqReadyState :: Eq ReadyState where
  eq = gEq

instance ordReadyState :: Ord ReadyState where
  compare = gCompare

instance showReadyState :: Show ReadyState where
  show = gShow

instance boundedReadyState :: Bounded ReadyState where
  bottom = Connecting
  top    = Closed

instance enumReadyState :: Enum ReadyState where
  cardinality = Cardinality 4
  toEnum      = toEnumReadyState
  fromEnum    = fromEnumReadyState
  succ        = defaultSucc toEnumReadyState fromEnumReadyState
  pred        = defaultPred toEnumReadyState fromEnumReadyState

toEnumReadyState :: Int -> Maybe ReadyState
toEnumReadyState 0 = Just Connecting
toEnumReadyState 1 = Just Open
toEnumReadyState 2 = Just Closing
toEnumReadyState 3 = Just Closed
toEnumReadyState _ = Nothing

fromEnumReadyState :: ReadyState -> Int
fromEnumReadyState Connecting = 0
fromEnumReadyState Open       = 1
fromEnumReadyState Closing    = 2
fromEnumReadyState Closed     = 3

-- | A numeric value indicating the status code explaining why the connection is being closed.
-- | See [the list of status codes](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent#Status_codes).
type Code = Int

-- | A human-readable string explaining why the connection is closing. This
-- | string must be no longer than 123 bytes of UTF-8 text (not characters).
type Reason = String

-- | A synonym for URL strings.
type URL = String

-- | A synonym for message strings.
type Message = String
