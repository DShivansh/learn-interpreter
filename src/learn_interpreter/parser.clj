(ns learn-interpreter.parser
  (:require [learn-interpreter.ast :as ast]
            [learn-interpreter.tokenizer :as tokenizer]
            [clojure.pprint :as p])
  (:import [learn_interpreter.ast LetStatement Identifier ReturnStatement
            ExpressionStatement IntegerLiteral PrefixExpression InfixExpression BooleanExpression IfExpression BlockStatement FunctionLiteral CallExpression]
           [learn_interpreter.tokenizer Token]))

(declare parse-expression)
(declare parse-statements)

(def precedence-map {
                 'IOTA -1
                 'LOWEST 1
                 'EQUALS 2
                 'LESSGREATER 3
                 'SUM 4
                 'PRODUCT 5
                 'PREFIX 6
                 'CALL 7
                     })

(def precedences {
                  'EQ (get precedence-map 'EQUALS)
                  'NOT_EQ (get precedence-map 'EQUALS)
                  'LT (get precedence-map 'LESSGREATER)
                  'GT (get precedence-map 'LESSGREATER)
                  'PLUS (get precedence-map 'SUM)
                  'SLASH (get precedence-map 'PRODUCT)
                  'ASTERISK (get precedence-map 'PRODUCT)
                  'LPAREN (get precedence-map 'CALL)
                  })

(defn get-precedence [token-type]
  (get precedences token-type (get precedence-map 'LOWEST)))

(defn parse-identifier [[first & rest]]
  [rest (Identifier. (.Type first) (.Literal first))])

(defn parse-integer-literal [[first & rest]]
  ;; (println)
  ;; (println (str " parsing integer literal first is " first " rest is " rest))
  [rest (IntegerLiteral. (.Type first) (Integer/parseInt (.Literal first)))])

(defn parse-prefix-expression [[first & rest]]
  ;; (println)
  ;; (println (str " parsing prefix expression first is " first " rest is " rest))
      (println "~~~~~~~~~~~~*********~~~~~~~~")
    (println "calling parse-expression from parse-prefix-expression")
    (println)
  (let [[rest-from-parse-expression parsed-expression] (parse-expression rest 6)]
    (println)
    (println " prefix expression after the parse expression is called ")
    [rest-from-parse-expression (PrefixExpression. (.Type first) (.Literal first) parsed-expression)]))

(defn parse-infix-expression [[first-token & rest-tokens] left-ast-expression]
  (println)
      (println "calling parse-expression from parse-infix-expression")
  (let [precedence (get-precedence (.Type first-token)) [left-tokens right-ast-expression] (parse-expression rest-tokens precedence)]
    (println)
    (println "parse-infix-expression"  " first-token is " first-token " parse-infix-expression " rest-tokens)
    (println)

    [left-tokens (InfixExpression. (.Type first-token) left-ast-expression (.Literal first-token) right-ast-expression)]))

(defn parse-boolean-expression [[first-token & rest-tokens]]
  [rest-tokens (BooleanExpression. (.Type first-token) (= 'TRUE (.Type first-token)))])

(defn parse-grouped-expression [[first-token & rest-tokens]]
  (let [[[returned-first-token & returned-rest-tokens] exp] (parse-expression rest-tokens (get-precedence 'LOWEST))]
    (cond
      (not= (.Type returned-first-token) 'RPAREN) (throw (Exception. "Right param not found"))
      :else [returned-rest-tokens exp])))

(defn parse-block-expression [[first-block-token & rest-block-tokens]]
  (loop [[first-token & rest-tokens] rest-block-tokens statements [] rest-block-tokens rest-block-tokens]
    (println)
    (println " first block token is " first-block-token)
    (println " inside parse-block-expression rest block tokens are " rest-block-tokens)
    (println " first token is " first-token)
    (println " rest-tokens are " rest-tokens)
    (cond
      (= (.Type first-token) 'RBRACE) (do
                                        (println)
                                        (println " inside the parse-block-expression ")
                                        (println " rest-tokens are " rest-tokens)
                                        (println " rest block tokens are " rest-block-tokens)
                                        [rest-tokens (BlockStatement. (.Type first-block-token) statements)])
      (= (.Type first-token) 'EOF) [rest-block-tokens (BlockStatement. (.Type first-block-token) statements)]
      :else (let [[left-tokens-after-parsing-statements statements-got] (parse-statements rest-block-tokens)]
              (recur left-tokens-after-parsing-statements (conj statements statements-got) left-tokens-after-parsing-statements)))))

(defn parse-if-expression [[if-token & rest-tokens]]
  (let [if-token if-token [left-paren & if-condition-expression] rest-tokens]
    (println)
    (println " parsing if expression ")
    (println " if token is " if-token " left-paren " left-paren " if condition expression " if-condition-expression)
    (cond
      (not= (.Type left-paren) 'LPAREN) (throw (Exception. "left paren is necessary after the if keyword"))
      :else (let [[[right-paren & rest-block-statements] if-condition-expression] (parse-expression if-condition-expression (get-precedence 'LOWEST)) left-brace (first rest-block-statements)]
              (println)
              (println " parse if expression function after parse expression condition expression is " if-condition-expression)
              (println)
              (println " right paren is " right-paren " rest block statements are " rest-block-statements)
              (cond
                (not= 'RPAREN (.Type right-paren)) (throw (Exception. "Right paren is missing in if condition which is necessary"))
                (not= 'LBRACE (.Type left-brace)) (throw (Exception. "Left brace should be present after condition"))
                :else (let [[left-tokens-after-parsing-block parse-block-expressions] (parse-block-expression rest-block-statements)]
                        (cond
                          (= 'ELSE (.Type (first left-tokens-after-parsing-block))) (let [tokens-for-else (rest left-tokens-after-parsing-block)]
                                                                                      (cond
                                                                                        (not= 'LBRACE (.Type (first tokens-for-else))) (throw (Exception. "else block should start with left paren"))
                                                                                        :else (let [[left-tokens-after-parsing-else-block parse-else-block-expression] (parse-block-expression tokens-for-else)]
                                                                                                [left-tokens-after-parsing-else-block (IfExpression. (.Type if-token) if-condition-expression parse-block-expressions parse-else-block-expression)])))
                          :else [left-tokens-after-parsing-block (IfExpression. (.Type if-token) if-condition-expression parse-block-expressions nil)])))))))

(defn parse-function-parameters-helper [parameters]
  (println)
  (println "parse-function-parameters-helper is called")
  (println " parameters passed are " parameters)
  (loop [[first-parameter & rest-parameters] parameters identifiers [] params parameters]
    (cond
      (= 'COMMA (.Type first-parameter)) (recur rest-parameters identifiers rest-parameters)
      (= 'IDENT (.Type first-parameter)) (recur rest-parameters (conj identifiers (Identifier. (.Type first-parameter) (.Literal first-parameter))) rest-parameters)
      :else [params identifiers])))


(defn parse-function-parameters [parameters]
  (let [identifiers []]
    (println)
    (println " parse function parameters first parameter is " (first parameters))
    (println " rest parameters are " (rest parameters))
    (println)
    (cond
      (= 'RPAREN (.Type (first parameters))) [(rest parameters) identifiers]
      :else (let [[tokens-left parsed-identifiers] (parse-function-parameters-helper parameters)]
              (cond
                (not= 'RPAREN (.Type (first tokens-left))) (throw (Exception. "function parameters must end with a right paran"))
                :else [(rest tokens-left) parsed-identifiers])))))

 
(defn parse-function-literal [[function-token & rest-function-tokens]]
  (cond
    (not= 'LPAREN (.Type (first rest-function-tokens))) (throw (Exception. "function keyword must be followed by a left parenthesis which looks like '('"))
    :else (let [[tokens-left-after-parsing-function-parameters parsed-parameters] (parse-function-parameters (rest rest-function-tokens))]
            (cond
              (not= 'LBRACE (.Type (first tokens-left-after-parsing-function-parameters))) (throw (Exception. "left brace is required after function prameters"))
              :else (let [[tokens-left-after-parsing-block parsed-block] (parse-block-expression tokens-left-after-parsing-function-parameters)]
                      [tokens-left-after-parsing-block (FunctionLiteral. (.Type function-token) parsed-parameters parsed-block)])))))

(defn parse-call-arguments-helper
  "tokens will be like this <expression 1>, <expression 2> ..... <expression n>)"
  [tokens]
  (println)
  (println "parse-call-arguments-helper")
  (println " tokens are " tokens)
  (loop [[first-token & rest-tokens] tokens arguments [] tokens1 tokens]
    (cond
      (= 'COMMA (.Type first-token)) (recur rest-tokens arguments rest-tokens)
      :else (cond
              (= 'RPAREN (.Type first-token)) [tokens1 arguments]
              :else (let [[left-tokens-after-parsing-expression parsed-expression] (parse-expression tokens1 (get-precedence 'LOWEST))]
                      (cond
                        (= 'RPAREN (.Type (first left-tokens-after-parsing-expression))) (recur left-tokens-after-parsing-expression (conj arguments parsed-expression) left-tokens-after-parsing-expression)
                        (= 'COMMA (.Type (first left-tokens-after-parsing-expression))) (recur left-tokens-after-parsing-expression (conj arguments parsed-expression) left-tokens-after-parsing-expression)
                        :else (throw (Exception. "function call must end with a closing bracket"))))))))


(defn parse-call-arguments [tokens]
  (println)
  (println "parse-call-arguments")
  (println " first token is " (first tokens))
  (println)
  (println " rest-tokens are " (rest tokens))
  (cond
    (= (.Type (first tokens)) 'RPAREN) [(rest tokens) []]
    :else (let [[left-tokens parsed-arguments] (parse-call-arguments-helper tokens)]
            (println)
            (println " tokens left after parsing arguments are " left-tokens)
            (println)
            (println " parsed arguments are " parsed-arguments)
            (cond
              (not= (.Type (first left-tokens)) 'RPAREN) (throw (Exception. "function call arguments must end with right paren"))
              :else [(rest left-tokens) parsed-arguments]))))

(defn parse-call-expression [[first-token & rest-tokens] function-expression]
  (let [[tokens-left parsed-arguments] (parse-call-arguments rest-tokens)]
        [tokens-left (CallExpression. (.Type first-token) function-expression parsed-arguments)]))

(def functions-associated-with-tokens {
                                       'IDENT parse-identifier
                                       'INT parse-integer-literal
                                       'BANG parse-prefix-expression
                                       'MINUS parse-prefix-expression
                                       'TRUE parse-boolean-expression
                                       'FALSE parse-boolean-expression
                                       'LPAREN parse-grouped-expression
                                       'IF parse-if-expression
                                       'FUNCTION parse-function-literal
                                       })

(def infix-functions-associated-with-tokens {
                                             'PLUS  parse-infix-expression
                                             'MINUS parse-infix-expression
                                             'SLASH parse-infix-expression
                                             'ASTERISK parse-infix-expression
                                             'EQ parse-infix-expression
                                             'NOT_EQ parse-infix-expression
                                             'LT parse-infix-expression
                                             'GT parse-infix-expression
                                             'LPAREN parse-call-expression
                                             })

(defn parse-infix-expression-helper [tokens left-exp precedence]
  (loop [[first-token & rest-tokens] tokens internal-tokens tokens left-exp left-exp]
    (println)
    (println " parsing expression helper first is " first-token " rest is " rest-tokens " tokens is " internal-tokens)
    (println " first token is " (.Type first-token) " precedence passed is " precedence " precedence got is " (get-precedence (.Type first-token)))

    (cond
      (= 'EOF (.Type first-token)) [internal-tokens left-exp]
      (= 'SEMICOLON (.Type first-token)) [internal-tokens left-exp]
      (> precedence (get-precedence (.Type (first rest-tokens)))) (do
                                                            ;; (println)
                                                            ;; (println " inside precedence block ")
                                                            ;; (println " current-precedence " precedence " precedence-gotten " (get-precedence (.Type first-token)))
                                                            [internal-tokens left-exp])
      :else (let [infix-parse-function (get infix-functions-associated-with-tokens (.Type first-token))]
              ;; (println "")
              ;; (println " first token is " first-token " infix parse function is " infix-parse-function)
              (cond
                (nil? infix-parse-function) [internal-tokens left-exp]
                :else (let [[left-tokens infix-expression] (infix-parse-function internal-tokens left-exp)]
                        (println)
                        (println " got an infix function left tokens is " left-tokens " infix-expression is " infix-expression)
                        (recur left-tokens left-tokens infix-expression)))))))


(defn parse-expression [tokens precedence]
  (println)
  (println "parse-expression tokens are " tokens)
  (let [[first & rest] tokens func (functions-associated-with-tokens (.Type first))]
    (println " parsing expression first is " first " rest is " rest)
    (println " function got is " func)
    (cond
      (nil? func) (throw (Exception. "Cannot parse expression in parse-expression function"))
      :else (let [[left-tokens left-exp] (func tokens)]
              (println)
              (println " parse-expression left tokens are " left-tokens " left expression is " left-exp)
              (parse-infix-expression-helper left-tokens left-exp precedence)))))

;; (defn parse-expression [tokens precedence]
;;   (let [[first & rest] tokens func (functions-associated-with-tokens (.Type first))]
;;     ;; (println)
;;     ;; (println " parsing expression first is " first " rest is " rest)
;;     (cond
;;       (nil? func) (throw (Exception. "Cannot parse expression in parse-expression function"))
;;       :else (func tokens))))

(defn drop-till-semi-colon [tokens]
  (next (drop-while #(not= (.Type %) 'SEMICOLON) tokens)))

;; (defn parse-return-statement
;;   "This function will parse the return statement
;;   the structure of the `return` statement looks like This
;;   return <EXPRESSION>

;;   return : [<sequence of tokens to process> formed AST]
;;   "
;;   [tokens]
;;   (let [return-token (first tokens) tokens tokens tokens-to-return (drop-till-semi-colon tokens)]
;;     (cond
;;       (not= (.Type return-token) 'RETURN) (throw (Exception. "first token of the return statement should be return"))
;;       :else [tokens-to-return (ReturnStatement. 'RETURN nil)])))

(defn parse-return-statement [tokens]
  (let [[return-token & rest-return-tokens] tokens]
    (println)
    (println " return-token is " return-token)
    (println)
    (println " rest-return-tokens are " rest-return-tokens)
    (println)
    (cond
      (not= 'RETURN (.Type return-token)) (throw (Exception. "first token of the return statement should be return"))
      (nil? rest-return-tokens) (throw (Exception. "return statement must be followed by the expression"))
      :else (let [[rest-tokens-after-parsing-expression parsed-expression] (parse-expression rest-return-tokens (get-precedence 'LOWEST))]
              (println)
              (println " rest-tokens-after-parsing-expression " rest-tokens-after-parsing-expression)
              (println)
              (cond
                (= 'SEMICOLON (.Type (first rest-tokens-after-parsing-expression))) [(rest rest-tokens-after-parsing-expression) (ReturnStatement. 'RETURN parsed-expression)]
                :else [rest-tokens-after-parsing-expression (ReturnStatement. 'RETURN parsed-expression)])))))

;; (defn parse-let-statement
;;   "This function will take the tokens and parse the LET statement
;;   the structure of `let` statement looks Like
;;   let <IDENTIFIER> = <EXPRESSION>

;;   return : [<sequence of tokens to process> formed AST]
;;   "
;;   [tokens]
;;   (let [lett-token (first tokens) identifier-token (second tokens)
;;         assign-token (nth tokens 2) tokens tokens]
;;     (cond
;;       (not= (.Type identifier-token) 'IDENT) (throw (Exception. "after let statement there should be an identifier"))
;;       (not= (.Type assign-token) 'ASSIGN) (throw (Exception. "after the let statement and identifier we should have = sign"))
;;       :else (let [tokens-to-return (drop-till-semi-colon tokens)]
;;               [tokens-to-return (LetStatement. 'LET (Identifier. (.Type identifier-token) (.Literal identifier-token)) nil)]))))

(defn parse-let-statement
  [tokens]
  (let [[lett-token identifier-token assign-token & rest-tokens] tokens tokens-let tokens]
    (cond
      (nil? lett-token) (throw (Exception. "let statement must begin with let"))
      (nil? identifier-token) (throw (Exception. "let statement must have the identifier"))
      (nil? assign-token) (throw (Exception. "let statement must have assignment"))
      (nil? rest-tokens) (throw (Exception. "after the assignment operator there should be some expression"))
      (= 'SEMICOLON (first rest-tokens)) (throw (Exception. "after the assignment operator we should have some expression and not semicolon"))
      (not= (.Type identifier-token) 'IDENT) (throw (Exception. "after let statement there should be an identifier"))
      (not= (.Type assign-token) 'ASSIGN) (throw (Exception. "after the let statement and identifier we should have = sign"))
      :else (let [[rest-tokens-after-parsing-expression parsed-expression] (parse-expression rest-tokens (get-precedence 'LOWEST))]
              (cond
                (= 'SEMICOLON (.Type (first rest-tokens-after-parsing-expression))) [(rest rest-tokens-after-parsing-expression) (LetStatement. 'LET (Identifier. (.Type identifier-token) (.Literal identifier-token)) parsed-expression)]
                :else [rest-tokens-after-parsing-expression (LetStatement. 'LET (Identifier. (.Type identifier-token) (.Literal identifier-token)) parsed-expression)])))))



(defn parse-expression-statement
  [tokens]
  (println)
      (println " calling parse expression from parse-expression-statement")
  (let [[left-tokens output] (parse-expression tokens (get precedence-map 'LOWEST)) ans (ExpressionStatement. (:Token output) output)]
    (println)
    (println " parse expression statement output is " output " token is " (:Token output) " left-tokens are " left-tokens)
    (cond
      ;; (nil? left-tokens) [nil ans]
      (= (.Type (first left-tokens)) 'SEMICOLON) [(rest left-tokens) ans]
      :else [left-tokens ans])))

(defn parse-statements
  "This function will parse the tokens and return
   the tokens whose parsing is left and it will also return the statement that is formed"
  [tokens]
  ;; (println)
  ;; (println " parse statements is called ")
  (cond
    (= (.Type (first tokens)) 'LET) (parse-let-statement tokens)
    (= (.Type (first tokens)) 'RETURN) (let [[left-tokens parsed-statements] (parse-return-statement tokens)]
                                         (println)
                                         (println " left tokens after parsing the return statement " left-tokens)
                                         (println " parsed-statements are " parsed-statements)
                                         [left-tokens parsed-statements]
                                         )
    :else (parse-expression-statement tokens)))

(defn start
  "This function takes in the code as string"
  [input]
  (let [tokens (tokenizer/get-token input)]
    (loop [tokens tokens statements []]
      ;; (println)
      ;; (println (str " tokens are " tokens))
      (cond
        ;; (nil? tokens) statements
        (= (.Type (first tokens)) 'EOF) statements
        :else (let [[left-tokens statement-formed] (parse-statements tokens)]
                (recur left-tokens (conj statements statement-formed)))))))


;; (start "foobar;")
;; (tokenizer/get-token "5;")
;; (start "-5;")
;; (tokenizer/get-token "-5;")
;; (clojure.stacktrace/print-stack-trace (start "-5;"))
;; (parse-statements (tokenizer/get-token "-5;"))
;; ()
;; (-> "5;"
;;     (tokenizer/get-token)
;;     (p/pprint)
;;     (parse-expression))

;; (p/pprint (start "5 + 5;"))
;; (print (start "5 + 5;"))

;; (println "**************************")
;; (start "5 + 5;")


;; (start "a + b * c;")
;; (start "3 + 4; -5 * 5;")
;; (start "-5 * 5;")

;; (start "let a = 5;
;; let b = 10;
;; let c = 23;")

;; (start "return 5;
;; return 23;
;; return 34;")

;; (start "!-a;")
;; (start "1 + 2 + 3;")
;; (start "-1 + 2;")
;; (start "true;")
;; (start "3 > 5 == false;")
;; (start "true == true;")
;; (start "true != false;")
;; (start "(5 + 5) * 2;")
;; (start "1 + (2 + 3) + 4;")
;; (start "-(5 + 5);")
;; (start "if (x < y) { return x; }")
;; (start "if (x < y) { x }")
;; (start "if (x < y) { return x; } else { return y; }")
;; (tokenizer/get-token "fn (x, y) {x + y;}")
;; (start "fn (x, y, z) {x + y;}")
;; (start "fn () {return 1;}")
 
;; (start "add(1, 2*3, 4+5);")
;; (tokenizer/get-token "add(1, 2*3, 4+5);")
;; (start "return (1+2);")
;; (tokenizer/get-token "return 5;")
;; (start "let x = 5;")
;; (start "let y = true;")
;; (start "let foobar = y;")

