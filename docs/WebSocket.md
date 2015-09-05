## Module WebSocket

This module defines a simple low-level interface to the websockets API.

#### `WebSocket`

``` purescript
data WebSocket :: !
```

The effect associated with websocket connections.

#### `Socket`

``` purescript
data Socket :: *
```

A reference to a websocket.

#### `URI`

``` purescript
type URI = String
```

A synonym for URI strings.

#### `Message`

``` purescript
type Message = String
```

A synonym for message strings.

#### `mkWebSocket`

``` purescript
mkWebSocket :: forall e. URI -> Eff (ws :: WebSocket | e) Socket
```

Create a websocket object for a URI.

#### `onMessage`

``` purescript
onMessage :: forall e a. Socket -> (Message -> Eff (ws :: WebSocket | e) a) -> Eff (ws :: WebSocket | e) Unit
```

Register a callback for incoming messages.

#### `onError`

``` purescript
onError :: forall e a. Socket -> Eff (ws :: WebSocket | e) a -> Eff (ws :: WebSocket | e) Unit
```

Register a callback for `error` events.

#### `onOpen`

``` purescript
onOpen :: forall e a. Socket -> Eff (ws :: WebSocket | e) a -> Eff (ws :: WebSocket | e) Unit
```

#### `onClose`

``` purescript
onClose :: forall e a. Socket -> Eff (ws :: WebSocket | e) a -> Eff (ws :: WebSocket | e) Unit
```

Register a callback for `close` events.

#### `send`

``` purescript
send :: forall e. Socket -> Message -> Eff (ws :: WebSocket | e) Unit
```

Send a message to a websocket.


