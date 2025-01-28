(ns learn-interpreter.parser
  (:require [learn-interpreter.ast :as ast]
            [learn-interpreter.tokenizer :as tokenizer])
  (:import [learn_interpreter.ast LetStatement Identifier]
           [learn_interpreter.tokenizer Token]))


(defn parse-let-statement
  "This function will take the tokens and parse the let statement"
  [tokens]
  (println (str "tokens from the parse-let-statements" tokens))
  (let [lett-token (first tokens) identifier-token (second tokens)
        assign-token (nth tokens 2) tokens tokens]
    (cond
      (not= (.Type identifier-token) 'IDENT) nil
      (not= (.Type assign-token) 'ASSIGN) nil
      :else (let [tokens-to-return (next (drop-while #(not= (.Type %) 'SEMICOLON) tokens))]
              [tokens-to-return (LetStatement. 'LET (Identifier. (.Type identifier-token) (.Literal identifier-token)) nil)]))))

(defn parse-statements
  "This function will parse the tokens and return
   the tokens whose parsing is left and it will also return the statement that is formed"
  [tokens]
  (cond
    (= (.Type (first tokens)) 'LET) (parse-let-statement tokens)
    :else nil))

(defn start
  "This function takes in the code as string"
  [input]
  (let [tokens (tokenizer/get-token input)]
    (loop [tokens tokens statements []]
      (cond
        (= (.Type (first tokens)) 'EOF) statements
        :else (let [[left-tokens statement-formed] (parse-statements tokens)]
                (recur left-tokens (conj statements statement-formed)))))))

(start "let a = 5;")

(def a (tokenizer/get-token "let a = 5;"))
(parse-statements a)
(.Type (first a))


