#lang play

;; parse :: s-expr -> Expr
#| where
   <s-expr> ::= <num>
              | <sym>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
              | (list 'if0 <s-expr> <s-expr> <s-expr>)
              | (list 'with (list <sym> <s-expr>) <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c)
                            (parse t)
                            (parse f))]
    [(list 'with (list x e) b)
     (with x (parse e) (parse b))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]))
 
;; subst :: Expr symbol Expr
(define (subst expr sub-id val)
  (match expr
    [(num n) expr]
    [(add l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [(sub l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [(if0 c t f) (if0 (subst c sub-id val)
                      (subst t sub-id val)
                      (subst f sub-id val))]
    [(with bound-id named-expr body)
     (with bound-id
           (subst named-expr sub-id val)
           (if (symbol=? bound-id sub-id)
               body
               (subst body sub-id val)))]
    [(id x) (if (symbol=? x sub-id) val expr)]))
 
 
;; calc :: Expr -> number
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(if0 c t f)
     (if (zero? (calc c))
         (calc t)
         (calc f))]
    [(with bound-id named-expr bound-body)
     (calc (subst bound-body
                  bound-id
                  (num (calc named-expr))))]
    [(id x) (error 'calc "free identifier: ~a" x)]))
 
;; run :: s-expr -> number
(define (run prog)
  (calc (parse prog)))

#|

   ========================================
                   Tarea 3
   ========================================
   Nombre: 
   Rut: 

|#


#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <sym> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x ne b)
  (fun id body)
  (id s)
  (app fun-expr arg-expr))
 

;; --------- [ Parte 1 ] ---------

;; const? :: Expr -> bool

(define (const? expr)
  (match expr
    [(num n) #t]
    [(add l r) (const? l) (const? r)]
    [(sub l r) (const? l) (const? r)]
    [(if0 c t f) (const? c) (const? t) (const? f)]
    [(with _ ne b) (const? ne) (const? b)]
    [(fun _ _) #f]
    [(id _) #f]
    [(app _ _) #f]
    ))

;; fold-consts :: Expr -> Expr

#|(define (fold-consts expr)
  (if (const? expr) (num (calc expr)) (
  (match expr
    [(num n) (num n)]
    [(id expr) (id expr)]
    [(add l r) (add (fold-consts l) (fold-consts r))]
    [(sub l r) (sub (fold-consts l) (fold-consts r))]
    [(if0 c t f) (if0 (fold-consts c)
                            (fold-consts t)
                            (fold-consts f))]
    [(with x ne b)
     (with x (fold-consts ne) (fold-consts b))]
    [(fun x b) (fun x (fold-consts b))]
    [(app f a) (app (fold-consts f) (fold-consts a))]))))
|#

#|
(define (fold-consts expr)
  (if (const? expr) (num (calc expr)) (fold-consts expr)))
|#

(define (fold-consts expr)
  (match expr
    [(num n) (num n)]
    [(id expr) (id expr)]
    [(add l r) (if (const? expr) (num (calc expr)) (add (fold-consts l) (fold-consts r)))]
    [(sub l r) (if (const? expr) (num (calc expr)) (sub (fold-consts l) (fold-consts r)))]
    [(if0 c t f) (if (const? expr) (num (calc expr)) ((if0 (fold-consts c)
                                                           (fold-consts t)
                                                           (fold-consts f))))]
    [(with x ne b) (with x (fold-consts ne) (fold-consts b))]
    [(fun x b) (fun x (fold-consts b))]
    [(app f a) (app f a)]))


;; --------- [ Parte 2 ] ---------

;; propagate-consts :: Expr -> Expr


;; --------- [ Parte 3 ] ---------

;; cf&p :: Expr -> Expr
