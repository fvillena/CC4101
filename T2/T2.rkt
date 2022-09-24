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

#| PARTE A |#
;; check-table :: Cmd -> Boolean / Error




#| PARTE B |#
;; check-arity :: Cmd -> Boolean / Error



#| PARTE C |#
;; check-column :: Cmd -> Boolean / Error



#| PARTE D |#
;; static-check :: Cmd -> Boolean / Error



#| PARTE F |#
;; end-time :: TaskSchedule string -> integer



#| ==============================
            EJERCICIO 3
   ============================== |#

#| PARTE A |#
;; interp-cmd :: Cmd Env -> List[Reg] 

 
#| PARTE B |#
;; run :: <s-Cmd> -> List[Reg] / Error

