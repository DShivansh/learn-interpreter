(ns learn-interpreter.tokenizer
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(def data-file (io/resource "input.lox"))
(def input (slurp data-file))
(println "#####################")
(print input)

(defrecord Token [Type Literal])

(def tokens {
             "=" 'ASSIGN
             "+" 'PLUS
             "-" 'MINUS
             "!" 'BANG
             "*" 'ASTRISK
             "/" 'SLASH
             "<" 'LT
             ">" 'GT
             "==" 'EQ
             "!=" 'NOT_EQ
             "," 'COMMA
             ";" 'SEMICOLON
             "(" 'LPAREN
             ")" 'RPAREN
             "{" 'LBRACE
             "}" 'RBRACE
             })

(def keywords {
               "fn" 'FUNCTION
               "let" 'LET
               "true" 'TRUE
               "false" 'FALSE
               "if" 'IF
               "else" 'ELSE
               "return" 'RETURN
               })

(def does-not-matter #{\newline \space \tab})

(defn identify-identifier-or-keyword [ans token-type]
  (cond
    (contains? keywords ans) (Token. (get keywords ans) ans)
    :else (Token. token-type ans)))

(defn split-at-whitespace [input func token-type]
  (loop [[first & second] input ans ""]
    (cond
      (or (contains? does-not-matter first) (contains? tokens (str first)) (func first))
      [(identify-identifier-or-keyword ans token-type) (apply str first second)]
      :else (recur second (str ans first)))))

(defn is-digit [input]
  (Character/isDigit input))

(defn is-letter [input]
  (Character/isLetter input))

(defn move-lazy [input]
  (loop [[first second & rest] input ans []]
    (cond
      (contains? does-not-matter first) (recur (str second (apply str rest)) ans)
      (nil? first) (list nil (conj ans (Token. 'EOF "")))
      (nil? second) (list "" (conj ans (Token. (get tokens (str first)) (str first))))
      (contains? tokens (str first second)) (list rest (conj ans (Token. (get tokens (str first second)) (str first second))))
      (contains? tokens (str first)) (list (str second (apply str rest)) (conj ans (Token. (get tokens (str first)) (str first))))
      (or (= first \") (Character/isLetter first) (= first \_))
      (let [[token left] (split-at-whitespace (str first second (apply str rest)) is-digit 'IDENT)]
        (list left (conj ans token)))
      (Character/isDigit first)
      (let [[token left] (split-at-whitespace (str first second (apply str rest)) is-letter 'INT)]
        (list left (conj ans token))))))

(defn get-token [input]
  (loop [code-str input ans []]
    (let [[rest token] (move-lazy code-str)]
    (cond
      (nil? rest) (concat ans token)
      :else (recur rest (concat ans token))))))

(println input)

(pp/pprint (get-token input))
(print (get-token input))
(println "###############")
(pp/pprint (get-token input))




