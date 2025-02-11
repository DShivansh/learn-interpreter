(ns learn-interpreter.ast)

(defrecord Program [Statements])

(defrecord Identifier [Token Value])
(defrecord LetStatement [Token Name Value])
(defrecord ReturnStatement [Token ReturnValue])
(defrecord ExpressionStatement [Token Expression])
(defrecord IntegerLiteral [Token ^int Value])
(defrecord PrefixExpression [Token Operator RightExpression])
(defrecord InfixExpression [Token LeftExpression Operator RightExpression])

