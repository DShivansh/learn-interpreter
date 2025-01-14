(ns learn-interpreter.tokenizer
  (:require [clojure.java.io :as io]))

(def data-file (io/resource "input.lox"))

(def tokens {"(" 'LEFT_PAREN
             ")" 'RIGHT_PAREN
             "{" 'LEFT_BRACE
             "}" 'RIGHT_BRACE
             "," 'COMMA
             "." 'DOT
             "-" 'MINUS
             ";" 'SEMI_COLON
             "*" 'STAR
             "!" 'BANG
             "!=" 'BANG_EQUAL
             "<" 'LESS
             "<=" 'LESS_EQUAL
             ">" 'GREATER
             ">=" 'GREATER_EQUAL
             "/" 'SLASH
             "\"" 'OPENING_QUOTE
             "and" 'AND
             "class" 'CLASS
             "else" 'ELSE
             "false" 'FALSE
             "for" 'FOR
             "fun" 'FUN
             "if" 'IF
             "nil" 'NIL
             "or" 'OR
             "print" 'PRINT
             "return" 'RETURN
             "super" 'SUPER
             "this" 'THIS
             "true" 'TRUE
             "var" 'VAR
             "while" 'WHILE
             })

(def input (slurp data-file))


(defn move-forward-till
  "move-forward-till function will move till max-iteration or till the token
   is find in mp"
  [input max-iteration mp]
  (loop ))

(defn move [input]
  (loop [[first second & rest] input ans []]
    (cond
      (nil? first) ans
      (nil? second) (conj ans first)
      (contains? tokens (str first second)) (recur rest (conj ans (str first second)))
      (contains? tokens (str first)) (recur (str second rest) (conj ans (str first)))
      (Character/isLetter first (recur (move-forward-till (str first second rest) 5 tokens))))))


