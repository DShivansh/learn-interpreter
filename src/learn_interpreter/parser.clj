(ns learn-interpreter.parser
  (:require [learn-interpreter.ast :as ast]
            [learn-interpreter.tokenizer :as tokenizer])
  (:import [learn_interpreter.ast LetStatement Identifier ReturnStatement ExpressionStatement]
           [learn_interpreter.tokenizer Token]))

(defn parse-identifier [[first & rest]]
  [rest (Identifier. (.Type first) (.Literal first))])

(def precedence-map {
                 'LOWEST 1
                 'EQUALS 2
                 'LESSGREATER 3
                 'SUM 4
                 'PRODUCT 5
                 'PREFIX 6
                 'CALL 7
                 })

(def functions-associated-with-tokens {
                                       'IDENT parse-identifier
                                       })

(defn parse-expression [tokens precedence]
  (let [[first & rest] tokens func (functions-associated-with-tokens (.Type first))]
    (cond
      (nil? func) (throw (Exception. "Cannot parse expression in parse-expression function"))
      :else (func tokens))))

(defn drop-till-semi-colon [tokens]
  (next (drop-while #(not= (.Type %) 'SEMICOLON) tokens)))

(defn parse-return-statement
  "This function will parse the return statement
  the structure of the `return` statement looks like This
  return <EXPRESSION>

  return : [<sequence of tokens to process> formed AST]
  "
  [tokens]
  (let [return-token (first tokens) tokens tokens tokens-to-return (drop-till-semi-colon tokens)]
    (cond
      (not= (.Type return-token) 'RETURN) (throw (Exception. "first token of the return statement should be return"))
      :else [tokens-to-return (ReturnStatement. 'RETURN nil)])))

(defn parse-let-statement
  "This function will take the tokens and parse the let statement
  the structure of `let` statement looks Like
  let <IDENTIFIER> = <EXPRESSION>

  return : [<sequence of tokens to process> formed AST]
  "
  [tokens]
  (let [lett-token (first tokens) identifier-token (second tokens)
        assign-token (nth tokens 2) tokens tokens]
    (cond
      (not= (.Type identifier-token) 'IDENT) (throw (Exception. "after let statement there should be an identifier"))
      (not= (.Type assign-token) 'ASSIGN) (throw (Exception. "after the let statement and identifier we should have = sign"))
      :else (let [tokens-to-return (drop-till-semi-colon tokens)]
              [tokens-to-return (LetStatement. 'LET (Identifier. (.Type identifier-token) (.Literal identifier-token)) nil)]))))

(defn parse-expression-statement
  [tokens]
  (let [[left-tokens output] (parse-expression tokens (get precedence-map 'LOWEST)) ans (ExpressionStatement. (.Type (first tokens)) output)]
    (cond
      (= (.Type (first left-tokens)) 'SEMICOLON) [(rest left-tokens) ans]
      :else [left-tokens ans])))

(defn parse-statements
  "This function will parse the tokens and return
   the tokens whose parsing is left and it will also return the statement that is formed"
  [tokens]
  (cond
    (= (.Type (first tokens)) 'LET) (parse-let-statement tokens)
    (= (.Type (first tokens)) 'RETURN) (parse-return-statement tokens)
    :else (parse-expression-statement tokens)))

(defn start
  "This function takes in the code as string"
  [input]
  (let [tokens (tokenizer/get-token input)]
    (loop [tokens tokens statements []]
      (cond
        (= (.Type (first tokens)) 'EOF) statements
        :else (let [[left-tokens statement-formed] (parse-statements tokens)]
                (recur left-tokens (conj statements statement-formed)))))))



(start "foobar;")

;; (start "let a = 5;
;; let b = 10;
;; let c = 23;")

;; (start "return 5;
;; return 23;
;; return 34;")



