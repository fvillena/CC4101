;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ELEMENTARY ARITHMETICAL EXPRESSIONS        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang play


#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
|#
;; Inductive type for representing
;; arithmetical expressions 
(deftype Expr
  (num n)
  (add l r)
  (sub l r))


;; calc :: Expr -> number
;; evaluates an arithmetical expression
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]))


;; concrete syntax used for
;; writing arithmetical expressions
#|
<s-expr> ::= <num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
|#


;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]))


;; run :: s-expr -> number
;; evaluates an arithmetical expression
;; given in concrete syntax
(define (run prog)
  (calc (parse prog)))


;; some testing...
(define my-s-expr '(+ (- 6 8) (+ (- 5 7) (- 5 0))))
(printf "Input expression:  ")
 my-s-expr
(printf "Abstract representation:  ")
(parse my-s-expr) 
(printf "Evaluation:  ")
(run my-s-expr)
