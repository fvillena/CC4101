#lang play
(print-only-errors #t)

#|
    PREGUNTA #2
    Booleanizate (?
|#

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
|#
;; Inductive type for representing arith-
;; metical expressions with conditionals
;; and identifiers
(deftype ExprP2
  (num n)
  (add l r)
  (sub l r))

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]))


;; eval :: Expr -> number
;; evaluates arithmetical expressions
;; with conditionals and identifiers
(define (eval expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (eval l) (eval r))]
    [(sub l r) (- (eval l) (eval r))]))


;; run :: s-expr -> number
;; evaluates an arithmetical expression with
;; conditionals given in concrete syntax
(define (run prog)
  (eval (parse prog)))


(test (run '5) 5)
(test (run '(+ 5 5)) 10)








#|
    PREGUNTA #3
    Freevars: Capturando identificadores sin binding.
|#

(deftype ExprP3
  (numm n)  
  (addd l r)
  (subb l r)
  (with id named-expr body)
  (id x))

;; parseP3 :: s-expr -> ExprP3
;; converts s-expressions into Exprs
(define (parseP3 s-expr)
  (match s-expr
    [ n #:when (number? n) (numm n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (addd (parseP3 l) (parseP3 r))]
    [(list '- l r) (subb (parseP3 l) (parseP3 r))]
    [(list 'with (list x e) b) #:when (symbol? x)
         (with x (parseP3 e) (parseP3 b))]))

;; freevars :: ExprP3 -> List[Sym]
#;(define (freevars e)
    ...)

#;(test (freevars (parseP3 '{with {x 3} {with {y 5} {+ z y}}})) '(z))
#;(test (freevars (parseP3 '{with {x 1} x})) '())
#;(test (freevars (parseP3 '{+ x z})) '(x z))







#|
    PREGUNTA #4
    La venganza de los Polinomios
|#

#|
<Polynomial> ::= (nullp)
              |  (plus <Number> <Integer> >Polynomial>)
|#
(deftype Polynomial
  (nullp)
  (plus coef exp rem))


;; fold-poly :: A (Number Integer -> A) -> (Polynomial -> A)
;; Captura el patrón recursivo de un polinomio
#;(define (fold-poly a f)
    ...)

;; evalPoly :: Number -> (Polynomial -> Number)
;; Evalua un polinomio en un valor dado
#;(define (eval-poly v)
    (fold-poly ...))

#;(test ((eval-poly 3) (plus 2 3 (plus -6 2 (plus 2 1 (plus -1 0 (nullp)))))) 5)
                       