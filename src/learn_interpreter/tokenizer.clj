(ns learn-interpreter.tokenizer
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def data-file (io/resource "input.lox"))
(def input (slurp data-file))

(def tokens {"(" 'LEFT_PAREN
             ")" 'RIGHT_PAREN
             "{" 'LEFT_BRACE
             "}" 'RIGHT_BRACE
             "," 'COMMA
             "." 'DOT
             "+" 'PLUS
             "-" 'MINUS
             ";" 'SEMI_COLON
             "*" 'STAR
             "!" 'BANG
             "=" 'EQUAL
             "!=" 'BANG_EQUAL
             "<" 'LESS
             "<=" 'LESS_EQUAL
             ">" 'GREATER
             ">=" 'GREATER_EQUAL
             "/" 'SLASH
             "\"" 'OPENING_QUOTE
             })

(def keywords {
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

(def does-not-matter #{\newline \space})


(defn split-at-whitespace [input]
  (loop [[first & second] input ans ""]
    (cond
      (or (contains? does-not-matter first) (contains? tokens (str first))) [ans (apply str first second)]
      :else (recur second (str ans first)))))

(defn move [input]
  (loop [[first second & rest] input ans []]
    (cond
      (contains? does-not-matter first) (recur (str second (apply str rest)) ans)
      (nil? first) ans
      (nil? second) (conj ans (str first))
      (contains? tokens (str first second)) (recur rest (conj ans (str first second)))
      (contains? tokens (str first)) (recur (str second (apply str rest)) (conj ans (str first)))
      (Character/isLetterOrDigit first) (let [[token left] (split-at-whitespace (str first second (apply str rest)))]
                                          (recur left (conj ans token)))
      )
    )
  )


(move input)


