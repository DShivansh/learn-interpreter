(ns learn-interpreter.evaluator
  (:require [learn-interpreter.parser :as parser])
  (:import [learn_interpreter.ast LetStatement Identifier ReturnStatement
             ExpressionStatement IntegerLiteral PrefixExpression InfixExpression BooleanExpression IfExpression BlockStatement FunctionLiteral CallExpression]
   [learn_interpreter.object IntegerObj BooleanObj NullObj]))

(defn my-eval [ast-node]
  (println)
  (println " ast-node is " ast-node)
  (let [node ast-node ast-node-type (type node)]
    (println " type is " ast-node-type)
    (println (= (str learn_interpreter.ast.ExpressionStatement) (str ast-node-type)) " evaluation")
    (cond
      (= (str learn_interpreter.ast.ExpressionStatement) (str ast-node-type)) (do
                                                                    (println)
                                                                    (println " got inside expression block")
                                                                    (my-eval (.Expression node)))
      (= (str learn_interpreter.ast.IntegerLiteral) (str ast-node-type)) (do
                                                               (println)
                                                               (println " got inside IntegerLiteral block")
                                                               (IntegerObj. (.Value node)))
      (= (str learn_interpreter.ast.BooleanExpression) (str ast-node-type)) (do
                                                                  (println)
                                                                  (println " got inside BooleanExpression block")
                                                                  (println "node is " node)
                                                                  (println " value is " (.Value node))
                                                                  (BooleanObj. (.Value node)))
      :else (do
              (println "inside the else block")
              nil))))

(defn eval-statements
  "This function will receive the list of parsed Statements
  This will be the starting point of evaluating the expression
  this function will receive the list containing defrecords from learn_interpreter.ast"
  [statements]
  (loop [[first-statement & rest-statements] statements result nil]
    (cond
      (nil? first-statement) result
      :else (recur rest-statements (my-eval first-statement)))))


;; (eval-statements (parser/start "5;"))
;; (parser/start "5;")
;; (def a (parser/start "true;"))
;; (eval-statements a)
;; (eval-statements (parser/start "true;"))
;; (parser/start "true;")
;; (str learn_interpreter.ast.ExpressionStatement)


