(ns learn-interpreter.evaluator
  (:require [learn-interpreter.parser :as parser])
  (:import [learn_interpreter.ast LetStatement Identifier ReturnStatement
             ExpressionStatement IntegerLiteral PrefixExpression InfixExpression BooleanExpression IfExpression BlockStatement FunctionLiteral CallExpression]
   [learn_interpreter.object IntegerObj BooleanObj NullObj]))

(defn eval [ast-node]
  (let [node ast-node ast-node-type (type node)]
    (cond
      (= learn_interpreter.ast.ExpressionStatement ast-node-type) (eval (.Expression node))
      (= learn_interpreter.ast.IntegerLiteral ast-node-type) (IntegerObj. (.Value node))
      :else nil)))

(defn eval-statements
  "This function will receive the list of parsed Statements
  This will be the starting point of evaluating the expression
  this function will receive the list containing defrecords from learn_interpreter.ast"
  [statements]
  (loop [[first-statement & rest-statements] statements result nil]
    (cond
      (nil? first-statement) result
      :else (recur rest-statements (eval first-statement)))))


(eval-statements (parser/start "5;"))
(parser/start "5;")

