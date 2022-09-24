#lang play
(print-only-errors #t)

#|
    PREGUNTA #1
    Bindings
|#

;; -- a
(test 
 (let ([x 3])
   (let ([y x]) y))
 3)

;; -- b
(test
 (let ([x 3])
   (let ([x x]) x))
 3)

;; -- c
#;(let ([x 3])
    (+ (let ([y 5]) x)
       y))
;; Error, unbound identifier: y

;; -- d
#; (let ([x y])
     (+ 8 x))
;; Error, unbound identifier: y

;; -- e
(test
 (let ([x (let ([y 3]) y)])
    x)
 3)

;; -- f
#; (let ([x 5] [y (+ x 2)])
    (+ x y))
;; Error, unbound identifier: x

;; -- g
(test
 (let* ([a 7] [b 8] [c (* a b)])
   (- c a))
 49)

;; -- h
#; (let ([a 7] [b 8])
    (let ([c 12]) (add1 a))
     c)
;; Error, unbound identifier: c

;; -- i
(test
 (let ([a 10])
     (let ([c (let ([b (add1 a)])
               (let ([a (add1 b)])
                  (let ([d (add1 b)]) (add1 b))))] )
      (+ a (add1 c))))
 23)








#|
    PREGUNTA #2
    Booleanizate (?
|#

#|
<expr> ::= (true)
         | (num <num>)
         | (neg <expr>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (expAnd <expr> <expr>)

|#
;; Inductive type for representing arith-
;; metical expressions with conditionals
;; and identifiers
(deftype ExprP2
  (true)
  (num n)
  (neg p)
  (add l r)
  (sub l r)
  (expAnd l r))

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    ['true (true)]
    ['false (neg (true))]
    [ n #:when (number? n) (num n)]
    [(list '¬ x) (neg (parse x))]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '^ l r) (expAnd (parse l) (parse r))]
    [(list 'v l r) (neg (expAnd (neg (parse l)) (neg (parse r))))])) ;; DeMorgan

(define (handle-binops valL valR fun verif)
  (if (and (verif valL) (verif valR)) 
      (fun valL valR)
      (error "Type mismatch")))


;; eval :: Expr -> number
;; evaluates arithmetical expressions
;; with conditionals and identifiers
(define (eval expr)
  (match expr
    [(num n) n]
    [(true) #t]
    [(neg p)   (def valP (eval p))
               (if (boolean? valP) (not valP) (error "Type mismatch"))]
    [(add l r) (handle-binops (eval l) (eval r) + number?)]
    [(sub l r) (handle-binops (eval l) (eval r) - number?)]
    [(expAnd l r) (handle-binops (eval l) (eval r) (λ (x y) (and x y)) boolean?)]))


;; run :: s-expr -> number
;; evaluates an arithmetical expression with
;; conditionals given in concrete syntax
(define (run prog)
  (eval (parse prog)))

(test (run '5) 5)
(test (run '(+ 5 5)) 10)
(test (run '(^ true (¬ true))) #f)
(test (run '(^ true false)) #f)
(test (run '(v false (^ true true))) #t)
(test/exn (run '(+ 1 true)) "mismatch")








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
(define (freevars e)
  (define (help exp defined)
    (match exp
      [(numm n) (list)]
      [(addd l r) (append (help l defined)
                         (help r defined))]
      [(subb l r) (append (help l defined)
                         (help r defined))]
      [(with id ne b) (append (help ne defined)
                              (help b (cons id defined)))]
      [(id x) (if (member x defined)
                  '()
                  (list x))]))
  (help e '()))

(test (freevars (parseP3 '{with {x 3} {with {y 5} {+ z y}}})) '(z))
(test (freevars (parseP3 '{with {x 1} x})) '())
(test (freevars (parseP3 '{+ x z})) '(x z))







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
(define (fold-poly a f)
  (λ (p)
    (match p
      [(nullp) a]
      [(plus c g r) (f c g ((fold-poly a f) r))])))

;; evalPoly :: Number -> (Polynomial -> Number)
;; Evalua un polinomio en un valor dado
(define (eval-poly v)
  (fold-poly 0 (λ (c g acc) (+ (* c (expt v g)) acc))))


(test ((eval-poly 3) (plus 2 3 (plus -6 2 (plus 2 1 (plus -1 0 (nullp)))))) 5)
                       