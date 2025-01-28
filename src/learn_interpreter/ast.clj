(ns learn-interpreter.ast)

(defrecord Program [Statements])

(defrecord Identifier [Token Value])
(defrecord LetStatement [Token Name Value])
(defrecord ReturnStatement [Token ReturnValue])


