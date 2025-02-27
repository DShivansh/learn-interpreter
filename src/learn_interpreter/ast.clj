(ns learn-interpreter.ast)

(defrecord Program [Statements])

(defrecord Identifier [Token Value])
(defrecord LetStatement [Token Name Value])
(defrecord ReturnStatement [Token ReturnValue])
(defrecord ExpressionStatement [Token Expression])
(defrecord IntegerLiteral [Token ^int Value])
(defrecord PrefixExpression [Token Operator RightExpression])
(defrecord InfixExpression [Token LeftExpression Operator RightExpression])
(defrecord BooleanExpression [Token ^boolean Value])
(defrecord IfExpression [Token ;; here it will always be if
                         Condition Consequence Alternative])
(defrecord BlockStatement [Token ;; here this will be {
                           Statements ;; these statements will be the list of statements inside braces
                           ])
(defrecord FunctionLiteral [Token
                            Parameters ;; This is the list of Identifier record
                            BlockStatement]) ;; Here BlockStatement means the BlockStatement record

