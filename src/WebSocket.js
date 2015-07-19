/* global exports */
"use strict";

// module WebSocket

exports.mkWebSocket = function(url) {
    return function() {
        return new WebSocket(url);
    }
}

exports.onMessageImpl = function(socket, callback) {
    return function() {
        socket.onmessage = function(msg) {
            callback(msg.data)();
        }
    }
}

exports.onErrorImpl = function(socket, callback) {
    return function() {
        socket.onerror = callback;
    }
}

exports.onCloseImpl = function(socket, callback) {
    return function() {
        socket.onclose = callback;
    }
}

exports.onOpenImpl = function(socket, callback) {
    return function() {
        socket.onopen = callback;
    }
}

exports.sendImpl = function(socket, message) {
    return function() {
        socket.send(message);
    }
}
