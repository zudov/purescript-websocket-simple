/* global exports */
"use strict";

// module WebSocket

exports.specViolation = function(s) {
  throw new Error(s);
}

exports.newWebSocketImpl = function(url, protocols) {
  return function() {
    var socket = new WebSocket(url, protocols);
    var getSocketProp = function (prop) {
      return function() { return socket[prop]; }
    }
    var setSocketProp = function (prop) {
      return function(v) {
        return function() {
          socket[prop] = v;
          return {};
        }
      }
    }
    return { setBinaryType: setSocketProp("binaryType")
           , getBinaryType: getSocketProp("binaryType")
           , getBufferedAmount: getSocketProp("bufferedAmount")
           , setOnclose: setSocketProp("onclose")
           , getOnclose: getSocketProp("onclose")
           , setOnerror: setSocketProp("onerror")
           , getOnerror: getSocketProp("onerror")
           , setOnmessage: setSocketProp("onmessage")
           , getOnmessage: getSocketProp("onmessage")
           , setOnopen: setSocketProp("onopen")
           , getOnopen: getSocketProp("onopen")
           , setProtocol: setSocketProp("protocol")
           , getProtocol: getSocketProp("protocol")
           , getReadyState: getSocketProp("readyState")
           , getUrl: getSocketProp("url")
           , closeImpl:
              function(mCode) {
                return function(mReason) {
                  return function() {
                    socket.close(mCode.value0, mReason.value0);
                    return {};
                  }
                }
              }
           , sendImpl:
              function(message) {
                return function() {
                  socket.send(message);
                  return {};
                }
              }
           , getSocket: function () { return socket }
           };
  }
}
