(ns learn-interpreter.ast)

(defrecord Program [Statements])
;; statements will be the list of all the statements in language

;; (defrecord Node [Token Name Value Type])
;;Token -> Token Type
;;Name -> Name of the Token
;;Value -> Value after evaluating the EXPRESSION
;;Type -> whether it is a Statement or Expression


;; (defrecord Statement [Token Name Value])
;; ;; this statement will be all the statements that we will be forming
;; ;; still not sure but if something else is required then we will use that

;; (defrecord Expression [Token Name Value])


(defrecord Identifier [Token Name])
(defrecord LetStatement [Token Name Value])
;; ;; let <IDENTIFIER> = <EXPRESSION>
;; ;; Token will be token symbol which will be 'LET in our case
;; ;; Name will be the identifier
;; ;; Value will be the result of solving the expression on the right hand side

;; (defrecord Identifier [])

