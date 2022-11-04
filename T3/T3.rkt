#lang play

;; Código del intérprete extraído desde https://users.dcc.uchile.cl/~etanter/play-interps/

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

#|

   ========================================
                   Tarea 3
   ========================================
   Nombre: Fabián Villena
   Rut: 18.748.597-5

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
;; Tipo inductivo para representar expresiones
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
;; Función que indica si es que la expresión entregada
;; consiste en sólo constantes numéricas
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
;; Función que aplica constant folding recursivamente
(define (fold-consts expr)
  (match expr
    [(num n) (num n)]
    [(id expr) (id expr)]
    [(add l r) (if (const? expr) (num (calc expr empty-env)) (add (fold-consts l) (fold-consts r)))]
    [(sub l r) (if (const? expr) (num (calc expr empty-env)) (sub (fold-consts l) (fold-consts r)))]
    [(if0 c t f) (if (const? expr) (num (calc expr empty-env)) ((if0 (fold-consts c)
                                                           (fold-consts t)
                                                           (fold-consts f))))]
    [(with x ne b) (with x (fold-consts ne) b)]
    [(fun x b) (fun x (fold-consts b))]
    [(app f a) (app f a)]))

;; --------- [ Parte 2 ] ---------

;; propagate-consts :: Expr -> Expr
;; Función que aplica constant propagation recursivamente
(define (propagate-consts expr) (propagate-consts-env expr empty-env))

;; propagate-consts-env :: Expr Env -> Expr
;; Función auxiliar para usar propagate-consts con ambientes
(define (propagate-consts-env expr env)
  (match expr
    [(num n) (num n)] 
    [(add l r) (add (propagate-consts-env l env) (propagate-consts-env r env))]
    [(sub l r) (sub (propagate-consts-env l env) (propagate-consts-env r env))]
    [(if0 c t f) (if0 (propagate-consts-env c env) (propagate-consts-env t env) (propagate-consts-env f env))]
    [(fun x b) (fun x (if (const? b) (calc b env) b))]
    [(app f a) (app f a)]
    [(with x e b) (def new-env (extend-env x (if (const? e) (num (calc e env)) (id x)) env))
       (if (const? e) (propagate-consts-env b new-env) (with x (propagate-consts-env e new-env) (propagate-consts-env b new-env))) ]
    [(id x) (env-lookup x env) ]))

;; --------- [ Parte 3 ] ---------

;; cf&p :: Expr -> Expr
;; Función que aplica constant folding and propagation iterativamente
;; hasta alcanzar el punto fijo
(define (cf&p expr)
  (match* ((fold-consts expr) (propagate-consts (fold-consts expr)))
    [(a b) #:when (equal? a b) a]
    [(a b) (cf&p b)]))
