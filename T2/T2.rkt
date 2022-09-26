#lang play

#| ==============================
            EJERCICIO 1
   ============================== |#

#| PARTE A |#

#|
Cond ::=
    | < sym num
    | > sym num
    | = sym num
    | & Cond Cond
    | or Cond Cond
|#
;; Tipo inductivo para representar
;; filtros WHERE del lenguaje Cmd 
(deftype Cond
  (< s n)
  (> s n)
  (= s n)
  (& lc rc)
  (orb lc rc))

#|
Cmd ::=
    CREATE <sym> (list sym) Cmd
    INSERT (list sym) <sym> Cmd
    FROM <sym> SELECT regs WHERE Cond
|#
;; Tipo inductivo para representar
;; el lenguaje de consulta Cmd
;; que nos pormite crear tablas, ingresar registros
;; y finalmente consultar sobre estos registros
(deftype Cmd
  (CREATE table-name column-names cmd)
  (INSERT row table-name cmd)
  (FROM table-name SELECT regs WHERE cond))

#| PARTE B |#

;; sintaxis concreta usada para escribir
;; filtros WHERE del lenguaje Cmd
#|
s-Cond ::=
    | < sym num
    | > sym num
    | = sym num
    | & s-Cond s-Cond
    | or s-Cond s-Cond
|#

;; parse-cond :: s-Cond -> Cond
;; convierte s-Conds en Conds
(define (parse-cond s-Cond)
  (match s-Cond
    [(list '= l r) (= l r)]
    [(list '> l r) (> l r)]
    [(list '< l r) (< l r)]
    [(list '& l r) (& (parse-cond l) (parse-cond r))]
    [(list 'orb l r) (orb (parse-cond l) (parse-cond r))]
    ))

;; sintaxis concreta usada para escribir
;; el lenguaje de consulta Cmd
;; que nos pormite crear tablas, ingresar registros
;; y finalmente consultar sobre estos registros
#|
s-Cmd ::=
    CREATE <sym> (list sym) s-Cmd
    INSERT (list sym) <sym> s-Cmd
    FROM <sym> SELECT regs WHERE s-Cond
|#

;; parse :: s-Cmd -> Cmd
;; convierte s-Cmds en Cmds
(define (parse s-Cmd)
  (match s-Cmd
    [(list 'CREATE table-name column-names cmd) (CREATE table-name column-names (parse cmd))]
    [(list 'INSERT row table-name cmd) (INSERT row table-name (parse cmd))]
    [(list 'FROM table-name 'SELECT 'regs 'WHERE cond) (FROM table-name 'SELECT 'regs 'WHERE (parse-cond cond))]
    ))

#| ==============================
            EJERCICIO 2
   ============================== |#

;; *Código extraído de la clase 7*
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

#| PARTE A |#

;; check-table :: Cmd -> Boolean / Error
;; comprueba que las tablas donde se insertan o consultan datos
;; se encuentren previamente definidas
(define (check-table Cmd)
  (check-table-with-env Cmd empty-env))

;; check-table-with-env :: Cmd Env -> Boolean / Error
;; función auxiliar que complementa check-table para
;; el uso de ambientes
(define (check-table-with-env Cmd env)
  (match Cmd
    [(CREATE table-name column-names cmd) (check-table-with-env cmd (extend-env table-name 'dummy env))]
    [(INSERT row table-name cmd) (if (exists-in-env? table-name env) #t (error (format "Error Estatico A1: Registro ~a insertado en tabla indefinida ~a" row table-name))) (check-table-with-env cmd env) ]
    [(FROM table-name 'SELECT 'regs 'WHERE cond) (if (exists-in-env? table-name env) #t (error (format "Error Estatico A2: La tabla consultada ~a no se encuentra definida" table-name)))]
    ))

;; exists-in-env :: Symbol Env -> Boolean
;; función auxiliar que verifica si existe una
;; tabla en un ambiente
(define (exists-in-env? x env)
  (with-handlers ([exn:fail? (lambda (exn) #f)]) (env-lookup x env) #t))


#| PARTE B |#

;; check-arity :: Cmd -> Boolean / Error
;; comprueba que al insertar registros la aridad sea consistente
(define (check-arity Cmd)
  (check-arity-with-env Cmd empty-env))

;; check-arity-with-env :: Cmd Env -> Boolean / Error
;; función auxiliar que complementa check-arity para
;; el uso de ambientes
(define (check-arity-with-env Cmd env)
  (match Cmd
    [(CREATE table-name column-names cmd) (check-arity-with-env cmd (extend-env table-name column-names env))]
    [(INSERT row table-name cmd) (if (arity-match? row table-name env) #t (error (format "Error Estatico B: Registro ~a con aridad ~a insertado en tabla ~a de aridad ~a" row (length row) table-name (length (env-lookup table-name env))))) (check-arity-with-env cmd env) ]
    [(FROM table-name 'SELECT 'regs 'WHERE cond) #t]
    ))

;; arity-match? :: String Symbol Env -> Boolean
;; función auxiliar que verifica la consistencia
;; de en la aridad de una nueva fila y la aridad de la tabla
(define (arity-match? row table-name env)
  (equal? (length (env-lookup table-name env)) (length row)))

#| PARTE C |#

;; check-column :: Cmd -> Boolean / Error
;; verifica que los nombres consultados sean consistentes
(define (check-column Cmd)
  (check-column-with-env Cmd empty-env))

;; check-column-with-env :: Cmd Env -> Boolean / Error
;; función auxiliar que complementa check-column para
;; el uso de ambientes
(define (check-column-with-env Cmd env)
  (match Cmd
    [(CREATE table-name column-names cmd) (check-column-with-env cmd (extend-env table-name column-names env))]
    [(INSERT row table-name cmd) (check-column-with-env cmd env) ]
    [(FROM table-name 'SELECT 'regs 'WHERE cond) (check-cond-columns cond table-name env)]
    ))

;; check-cond-columns :: Cond Symbol Env -> Boolean / Error
;; verifica que las columnas consultadas en WHERE
;; estén presentes en la tabla
(define (check-cond-columns Cond table-name env)
  (match Cond
    [(or (< s n) (> s n) (= s n)) (if (column-in-table? s table-name env) #t (error (format "Error Estático C: La columna ~a no está definida en la tabla ~a" s table-name)))]
    [(or (orb lc rc) (& lc rc)) (check-cond-columns lc table-name env) (check-cond-columns rc table-name env)]))

;; column-in-table? :: String Symbol Env -> Boolean
;; función auxiliar que verifica si una columna está
;; en la tabla
(define (column-in-table? column table-name env)
  (if (member column (env-lookup table-name env)) #t #f))

#| PARTE D |#

;; static-check :: Cmd -> Boolean / Error
;; realiza los checking estáticos
;; check-table, check-arity y check-column
(define (static-check Cmd)
  (check-table Cmd)
  (check-arity Cmd)
  (check-column Cmd))

#| ==============================
            EJERCICIO 3
   ============================== |#

#| PARTE A |#

;; interp-cmd :: Cmd Env -> List[Reg]
;; interpreta las instrucciones de un programa en Cmd
(define (interp-cmd Cmd env)
  (match Cmd
    [(CREATE table-name column-names cmd) (interp-cmd cmd
                                                      (extend-env 'regs (list )
                                                                  (extend-env 'table-name table-name
                                                                              (extend-env 'column-names column-names
                                                                                          env))))]
    [(INSERT row table-name cmd)
     (define new-env (extend-env 'table-name (env-lookup 'table-name env)
                             (extend-env 'column-names (env-lookup 'column-names env)
                                         (extend-env 'regs (append (env-lookup 'regs env) (list row))
                                                     empty-env))))
     (interp-cmd cmd new-env)]
    [(FROM table-name 'SELECT 'regs 'WHERE cond) (interp-cond cond env)]
    ))

;; interp-cond :: Cond Env -> List[Reg]
;; interpreta las condiciones Cond
(define (interp-cond Cond env)
  (match Cond
    [(= s n)
     (define i (index-of (env-lookup 'column-names env) s) )
     (filter (lambda (e) (equal? (list-ref e i) n)) (env-lookup 'regs env))
     ]
    [(> s n)
     (define i (index-of (env-lookup 'column-names env) s) )
     (filter (lambda (e) (>= (list-ref e i) n)) (env-lookup 'regs env))
     ]
    [(< s n)
     (define i (index-of (env-lookup 'column-names env) s) )
     (filter (lambda (e) (<= (list-ref e i) n)) (env-lookup 'regs env))
     ]
    [(orb lc rc) (set-union (interp-cond lc env) (interp-cond rc env))]
    [(& lc rc) (set-intersect (interp-cond lc env) (interp-cond rc env))] 
    ))

;; *código extraído de https://stackoverflow.com/questions/57813823/how-to-filter-lists-in-a-list-in-scheme*
;; index-of :: List Void -> integer
;; retorna el índice del elemento dentro de una lista
(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

#| PARTE B |#

;; run :: <s-Cmd> -> List[Reg] / Error
;; realiza un análisis estático del programa y en caso de
;; no existir errores, lo ejecuta
(define (run Cmd)
  (static-check Cmd)
  (interp-cmd Cmd empty-env))