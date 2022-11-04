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


;; Interface of the Abstract Dada Type (ADT) for  
;; keeping track of the deferred substitutions

;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))
 
;; calc :: Expr -> number
(define (calc expr env)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l env) (calc r env))]
    [(sub l r) (- (calc l env) (calc r env))]
    [(if0 c t f)
     (if (zero? (calc c env))
         (calc t env)
         (calc f env))]
    [(with x e b) (def new-env (extend-env x (calc e env) env))
       (calc b new-env) ]
    [(id x) (env-lookup x env) ]))
 
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

(define (fold-consts expr)
  (match expr
    [(num n) (num n)]
    [(id expr) (id expr)]
    [(add l r) (if (const? expr) (num (calc expr empty-env)) (add (fold-consts l) (fold-consts r)))]
    [(sub l r) (if (const? expr) (num (calc expr empty-env)) (sub (fold-consts l) (fold-consts r)))]
    [(if0 c t f) (if (const? expr) (num (calc expr empty-env)) ((if0 (fold-consts c)
                                                           (fold-consts t)
                                                           (fold-consts f))))]
    [(with x ne b) (with x (fold-consts ne) (fold-consts b))]
    [(fun x b) (fun x (fold-consts b))]
    [(app f a) (app f a)]))

;; --------- [ Parte 2 ] ---------

;; propagate-consts :: Expr -> Expr

(define (propagate-consts expr) (propagate-consts-env expr empty-env))

(define (propagate-consts-env expr env)
  (match expr
    [(num n) (num n)] 
    [(add l r) (add (propagate-consts-env l env) (propagate-consts-env r env))]
    [(sub l r) (sub (propagate-consts-env l env) (propagate-consts-env r env))]
    [(if0 c t f) (if0 (propagate-consts-env c env) (propagate-consts-env t env) (propagate-consts-env f env))]
    [(with x e b) (def new-env (extend-env x (if (const? e) (num (calc e env)) (id x)) env))
       (if (const? e) (propagate-consts-env b new-env) (with x (propagate-consts-env e new-env) (propagate-consts-env b new-env))) ]
    [(id x) (env-lookup x env) ]
    [(fun x b) (fun x (if (const? b) (propagate-consts-env b env) b))]
    [(app f a) (app f a)]))

(propagate-consts (parse '{fun {y} {with {x 7} {+ x x}}}))

;; --------- [ Parte 3 ] ---------

;; cf&p :: Expr -> Expr
