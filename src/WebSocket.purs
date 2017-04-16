-- | This module defines a simple low-level interface to the websockets API.

module WebSocket
  ( WEBSOCKET()
  , WebSocket()
  , newWebSocket
  , Connection(..)
  , URL(..)
  , runURL
  , Message(..)
  , runMessage
  , runMessageEvent
  , Code(..)
  , runCode
  , Reason(..)
  , runReason
  , ReadyState(..)
  , Protocol(..)
  , runProtocol
  , BufferedAmount()
  , runBufferedAmount
  , BinaryType(..)
  ) where

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (Var, GettableVar, SettableVar, makeVar, makeGettableVar, makeSettableVar)
import Control.Monad.Except (runExcept)
import DOM.Event.EventTarget (eventListener, EventListener)
import DOM.Event.Types (Event)
import DOM.Websocket.Event.Types (CloseEvent, MessageEvent)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, defaultSucc, defaultPred, toEnum, Cardinality(..))
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Foreign.Index (readProp)
import Data.Function.Uncurried (runFn2, Fn2)
import Data.Functor.Contravariant (cmap)
import Data.Functor.Invariant (imap)
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable, Nullable)
import Prelude (class Ord, compare, class Eq, eq, class Bounded, class Show, Unit, (<$>), map, (<<<), ($))
import Unsafe.Coerce (unsafeCoerce)

foreign import specViolation :: forall a. String -> a

-- | The effect associated with websocket connections.
foreign import data WEBSOCKET :: Effect

-- | A reference to a WebSocket object.
foreign import data WebSocket :: Type

-- | Initiate a websocket connection.
newWebSocket :: forall eff. URL -> Array Protocol -> Eff (ws :: WEBSOCKET, err :: EXCEPTION | eff) Connection
newWebSocket url protocols = enhanceConnection <$> runFn2 newWebSocketImpl url protocols

foreign import newWebSocketImpl :: forall eff. Fn2 URL
                                                   (Array Protocol)
                                                   (Eff (ws :: WEBSOCKET, err :: EXCEPTION | eff) ConnectionImpl)

runMessageEvent :: MessageEvent -> Message
runMessageEvent event = case runExcept (readProp "data" (toForeign event)) of
                      Right x -> unsafeFromForeign x
                      Left _  -> specViolation "'data' missing from MessageEvent"

type ConnectionImpl =
  { setBinaryType
      :: forall eff. String -> Eff (ws :: WEBSOCKET | eff) Unit
  , getBinaryType
      :: forall eff. Eff (ws :: WEBSOCKET | eff) String
  , getBufferedAmount
      :: forall eff. Eff (ws :: WEBSOCKET | eff) BufferedAmount
  , setOnclose
      :: forall eff handlerEff. EventListener handlerEff -> Eff (ws :: WEBSOCKET | eff) Unit
  , setOnerror
      :: forall eff handlerEff. EventListener handlerEff -> Eff (ws :: WEBSOCKET | eff) Unit
  , setOnmessage
      :: forall eff handlerEff. EventListener handlerEff -> Eff (ws :: WEBSOCKET | eff) Unit
  , setOnopen
      :: forall eff handlerEff. EventListener handlerEff -> Eff (ws :: WEBSOCKET | eff) Unit
  , setProtocol
      :: forall eff. Protocol -> Eff (ws :: WEBSOCKET | eff) Unit
  , getProtocol
      :: forall eff. Eff (ws :: WEBSOCKET | eff) Protocol
  , getReadyState
      :: forall eff. Eff (ws :: WEBSOCKET | eff) Int
  , getUrl
      :: forall eff. Eff (ws :: WEBSOCKET | eff) URL
  , closeImpl
      :: forall eff. Nullable { code :: Code, reason :: Nullable Reason } -> Eff (ws :: WEBSOCKET, err :: EXCEPTION | eff) Unit
  , sendImpl
      :: forall eff. Message -> Eff (ws :: WEBSOCKET, err :: EXCEPTION | eff) Unit
  , getSocket
      :: forall eff. Eff (ws :: WEBSOCKET | eff) WebSocket
  }

coerceEvent :: forall a. Event -> a
coerceEvent = unsafeCoerce

enhanceConnection :: ConnectionImpl -> Connection
enhanceConnection c = Connection $
  { binaryType: imap toBinaryType fromBinaryType $ makeVar c.getBinaryType c.setBinaryType
  , bufferedAmount: makeGettableVar c.getBufferedAmount
  , onclose: cmap (eventListener <<< (_ `map` coerceEvent)) (makeSettableVar c.setOnclose)
  , onerror: cmap (eventListener <<< (_ `map` coerceEvent)) (makeSettableVar c.setOnerror)
  , onmessage: cmap (eventListener <<< (_ `map` coerceEvent)) (makeSettableVar c.setOnmessage)
  , onopen: cmap (eventListener <<< (_ `map` coerceEvent)) (makeSettableVar c.setOnopen)
  , protocol: makeVar c.getProtocol c.setProtocol
  , readyState: unsafeReadyState <$> makeGettableVar c.getReadyState
  , url: makeGettableVar c.getUrl
  , close: c.closeImpl (toNullable Nothing)
  , close': \code reason -> c.closeImpl (toNullable (Just { code, reason: toNullable reason }))
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
-- |   using calls to `send` but not yet transmitted to the
-- |   network. This value does not reset to zero when the
-- |   connection is closed; if you keep calling `send`,
-- |   this will continue to climb.
-- | - `onclose` -- An event listener to be called when the `Connection`'s
-- |   `readyState` changes to `Closed`.
-- | - `onerror` -- An event listener to be called when an error occurs.
-- | - `onmessage` -- An event listener to be called when a message is received
-- |   from the server.
-- | - `onopen` -- An event listener to be called when the `Connection`'s
-- |   readyState changes to `Open`; this indicates that the
-- |   connection is ready to send and receive data.
-- | - `protocol` -- A string indicating the name of the sub-protocol the server selected.
-- | - `readyState` -- The current state of the connection.
-- | - `url` -- The URL as resolved by during construction. This is always an absolute URL.
-- | - `close` -- Closes the connection or connection attempt, if any.
-- |   If the connection is already CLOSED, this method does nothing.
-- |   If `Code` isn't specified a default value of 1000 (indicating
-- |   a normal "transaction complete" closure) is assumed
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
  , close          :: forall eff. Eff (ws :: WEBSOCKET, err :: EXCEPTION | eff) Unit
  , close'         :: forall eff. Code -> Maybe Reason -> Eff (ws :: WEBSOCKET, err :: EXCEPTION | eff) Unit
  , send           :: forall eff. Message -> Eff (ws :: WEBSOCKET, err :: EXCEPTION | eff) Unit
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
newtype BufferedAmount = BufferedAmount Int

runBufferedAmount :: BufferedAmount -> Int
runBufferedAmount (BufferedAmount a) = a

derive instance genericBufferedAmount :: Generic BufferedAmount
instance eqBufferedAmount :: Eq BufferedAmount where
  eq (BufferedAmount a) (BufferedAmount b) = eq a b
instance ordBufferedAmount :: Ord BufferedAmount where
  compare (BufferedAmount a) (BufferedAmount b) = compare a b

-- | A string indicating the name of the sub-protocol.
newtype Protocol = Protocol String

runProtocol :: Protocol -> String
runProtocol (Protocol a) = a

derive instance genericProtocol :: Generic Protocol
instance eqProtocol :: Eq Protocol where
  eq (Protocol a) (Protocol b) = eq a b
instance ordProtocol :: Ord Protocol where
  compare (Protocol a) (Protocol b) = compare a b

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

instance boundedEnumReadyState :: BoundedEnum ReadyState where
  cardinality = Cardinality 4
  toEnum      = toEnumReadyState
  fromEnum    = fromEnumReadyState

instance enumReadyState :: Enum ReadyState where
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

-- | Should be either equal to 1000 (indicating normal closure) or in the range
-- | of 3000-4999.
newtype Code
  = Code Int

runCode :: Code -> Int
runCode (Code a) = a

derive instance genericCode :: Generic Code
instance eqCode :: Eq Code where
  eq (Code a) (Code b) = eq a b
instance ordCode :: Ord Code where
  compare (Code a) (Code b) = compare a b

-- | A human-readable string explaining why the connection is closing. This
-- | string must be no longer than 123 bytes of UTF-8 text (not characters).
newtype Reason = Reason String

runReason :: Reason -> String
runReason (Reason a) = a

derive instance genericReason :: Generic Reason

-- | A synonym for URL strings.
newtype URL = URL String

runURL :: URL -> String
runURL (URL a) = a

derive instance genericURL :: Generic URL

-- | A synonym for message strings.
newtype Message = Message String
derive instance genericMessage :: Generic Reason

runMessage :: Message -> String
runMessage (Message a) = a
