## Module WebSocket

This module defines a simple low-level interface to the websockets API.

#### `WEBSOCKET`

``` purescript
data WEBSOCKET :: !
```

The effect associated with websocket connections.

#### `WebSocket`

``` purescript
data WebSocket :: *
```

A reference to a WebSocket object.

#### `newWebSocket`

``` purescript
newWebSocket :: forall eff. URL -> Array Protocol -> Eff (ws :: WEBSOCKET | eff) Connection
```

Initiate a websocket connection. This would call `WebSocket` constructor and
would use it to create `Connection` object which can be used to safely
interface with WebSocket API from purescript. Raw `WebSocket` object is
kept in a closure and is accessible via `socket` label.

#### `Connection`

``` purescript
newtype Connection
  = Connection { binaryType :: forall eff. Var (ws :: WEBSOCKET | eff) BinaryType, bufferedAmount :: forall eff. ReadOnlyVar (ws :: WEBSOCKET | eff) BufferedAmount, onclose :: forall eff handlerEff. WriteOnlyVar (ws :: WEBSOCKET | eff) (EventListener handlerEff), onerror :: forall eff handlerEff. WriteOnlyVar (ws :: WEBSOCKET | eff) (EventListener handlerEff), onmessage :: forall eff handlerEff. WriteOnlyVar (ws :: WEBSOCKET | eff) (EventListener handlerEff), onopen :: forall eff handlerEff. WriteOnlyVar (ws :: WEBSOCKET | eff) (EventListener handlerEff), protocol :: forall eff. Var (ws :: WEBSOCKET | eff) Protocol, readyState :: forall eff. ReadOnlyVar (ws :: WEBSOCKET | eff) ReadyState, url :: forall eff. ReadOnlyVar (ws :: WEBSOCKET | eff) URL, close :: forall eff. Maybe Code -> Maybe Reason -> Eff (ws :: WEBSOCKET | eff) Unit, send :: forall eff. String -> Eff (ws :: WEBSOCKET | eff) Unit, socket :: forall eff. ReadOnlyVar (ws :: WEBSOCKET | eff) WebSocket }
```

#### `BinaryType`

``` purescript
data BinaryType
  = Blob
  | ArrayBuffer
```

The type of binary data being transmitted by the connection.

#### `BufferedAmount`

``` purescript
type BufferedAmount = Int
```

The number of bytes of data that have been buffered (queued but not yet transmitted)

#### `Protocol`

``` purescript
type Protocol = String
```

A string indicating the name of the sub-protocol.

#### `ReadyState`

``` purescript
data ReadyState
  = Connecting
  | Open
  | Closing
  | Closed
```

State of the connection.

##### Instances
``` purescript
instance showReadyState :: Show ReadyState
```

#### `Code`

``` purescript
type Code = Int
```

A numeric value indicating the status code explaining why the connection is being closed.
See [the list of status codes](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent#Status_codes).

#### `Reason`

``` purescript
type Reason = String
```

A human-readable string explaining why the connection is closing. This
string must be no longer than 123 bytes of UTF-8 text (not characters).

#### `URL`

``` purescript
type URL = String
```

A synonym for URL strings.

#### `Message`

``` purescript
type Message = String
```

A synonym for message strings.


