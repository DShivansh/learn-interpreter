(ns learn-interpreter.object)

(defrecord IntegerObj [^int Value])
(defrecord BooleanObj [^boolean Value])
(defrecord NullObj [])

