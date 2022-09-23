#lang play

#| ==============================
            EJERCICIO 1
   ============================== |#

#| PARTE A |#

#|
s-Cond ::=
    | < sym num
    | > sym num
    | = sym num
    | & s-Cond s-Cond
    | or s-Cond s-Cond
|#

(deftype Cond
  (< s n)
  (> s n)
  (= s n)
  (& lc rc)
  (orb lc rc))

#|
s-Cmd ::=
    CREATE <sym> (list sym) s-Cmd
    INSERT (list sym) <sym> s-Cmd
    FROM <sym> SELECT regs WHERE s-Cond
|#

(deftype Cmd
  (CREATE s ls c)
  (INSERT ls s c)
  (FROM s SELECT regs WHERE c))

#| PARTE B |#
;; parse :: s-Cmd -> Cmd

(define (parse s-Cmd)
  (match s-Cmd
    [(list 'CREATE s ls c) (let ([R (list )])(let ([table-name s]) (let ([column-names ls]) table-name)))]
    ))


(define my-s-Cmd '(CREATE A (a b c) (INSERT (1 2 3) A (INSERT (4 5 6) A (FROM A SELECT regs WHERE (= a 1))))))

(parse my-s-Cmd)

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

