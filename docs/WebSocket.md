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

Initiate a websocket connection.

#### `runMessageEvent`

``` purescript
runMessageEvent :: MessageEvent -> Message
```

#### `Connection`

``` purescript
newtype Connection
  = Connection { binaryType :: forall eff. Var (ws :: WEBSOCKET | eff) BinaryType, bufferedAmount :: forall eff. GettableVar (ws :: WEBSOCKET | eff) BufferedAmount, onclose :: forall eff handlerEff. SettableVar (ws :: WEBSOCKET | eff) (CloseEvent -> Eff handlerEff Unit), onerror :: forall eff handlerEff. SettableVar (ws :: WEBSOCKET | eff) (Event -> Eff handlerEff Unit), onmessage :: forall eff handlerEff. SettableVar (ws :: WEBSOCKET | eff) (MessageEvent -> Eff handlerEff Unit), onopen :: forall eff handlerEff. SettableVar (ws :: WEBSOCKET | eff) (Event -> Eff handlerEff Unit), protocol :: forall eff. Var (ws :: WEBSOCKET | eff) Protocol, readyState :: forall eff. GettableVar (ws :: WEBSOCKET | eff) ReadyState, url :: forall eff. GettableVar (ws :: WEBSOCKET | eff) URL, close :: forall eff. Maybe Code -> Maybe Reason -> Eff (ws :: WEBSOCKET | eff) Unit, send :: forall eff. Message -> Eff (ws :: WEBSOCKET | eff) Unit, socket :: forall eff. GettableVar (ws :: WEBSOCKET | eff) WebSocket }
```

- `binaryType` -- The type of binary data being transmitted by the connection.
- `bufferedAmount` -- The number of bytes of data that have been queued
  using calls to `send` but not yet transmitted to the
  network. This value does not reset to zero when the
  connection is closed; if you keep calling `send`,
  this will continue to climb.
- `onclose` -- An event listener to be called when the `Connection`'s
  `readyState` changes to `Closed`.
- `onerror` -- An event listener to be called when an error occurs.
- `onmessage` -- An event listener to be called when a message is received
  from the server.
- `onopen` -- An event listener to be called when the `Connection`'s
  readyState changes to `Open`; this indicates that the
  connection is ready to send and receive data.
- `protocol` -- A string indicating the name of the sub-protocol the server selected.
- `readyState` -- The current state of the connection.
- `url` -- The URL as resolved by during construction. This is always an absolute URL.
- `close` -- Closes the connection or connection attempt, if any.
  If the connection is already CLOSED, this method does nothing.
  If `Code` isn't specified a default value of 1000 (indicating
  a normal "transaction complete" closure) is assumed
- `send` -- Transmits data to the server.
- `socket` -- Reference to closured WebSocket object.

#### `BinaryType`

``` purescript
data BinaryType
  = Blob
  | ArrayBuffer
```

The type of binary data being transmitted by the connection.

#### `BufferedAmount`

``` purescript
newtype BufferedAmount
```

The number of bytes of data that have been buffered (queued but not yet transmitted)

##### Instances
``` purescript
Generic BufferedAmount
Eq BufferedAmount
Ord BufferedAmount
```

#### `runBufferedAmount`

``` purescript
runBufferedAmount :: BufferedAmount -> Int
```

#### `Protocol`

``` purescript
newtype Protocol
  = Protocol String
```

A string indicating the name of the sub-protocol.

##### Instances
``` purescript
Generic Protocol
Eq Protocol
Ord Protocol
```

#### `runProtocol`

``` purescript
runProtocol :: Protocol -> String
```

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
Generic ReadyState
Eq ReadyState
Ord ReadyState
Show ReadyState
Bounded ReadyState
Enum ReadyState
```

#### `Code`

``` purescript
newtype Code
  = Code Int
```

A numeric value indicating the status code explaining why the connection is being closed.
See [the list of status codes](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent#Status_codes).

##### Instances
``` purescript
Generic Code
Eq Code
Ord Code
```

#### `runCode`

``` purescript
runCode :: Code -> Int
```

#### `Reason`

``` purescript
newtype Reason
  = Reason String
```

A human-readable string explaining why the connection is closing. This
string must be no longer than 123 bytes of UTF-8 text (not characters).

##### Instances
``` purescript
Generic Reason
Generic Reason
```

#### `runReason`

``` purescript
runReason :: Reason -> String
```

#### `URL`

``` purescript
newtype URL
  = URL String
```

A synonym for URL strings.

##### Instances
``` purescript
Generic URL
```

#### `runURL`

``` purescript
runURL :: URL -> String
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

A synonym for message strings.

#### `runMessage`

``` purescript
runMessage :: Message -> String
```


