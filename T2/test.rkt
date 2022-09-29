#lang play
(require "T2.rkt")
(print-only-errors #t)

#| ==============================
           CASOS DE PRUEBA
   ============================== |#

#| CASOS BIEN FORMADOS |#

;; caso del enunciado
(define my-s-Cmd '(CREATE toki-toki-ti (mesa terremotos empanadas)
        (INSERT (3 5 4) toki-toki-ti
                (INSERT (2 2 4) toki-toki-ti
                        (INSERT (1 2 3) toki-toki-ti
                                (FROM toki-toki-ti SELECT regs WHERE (= empanadas 4)))))))
(define my-Cmd (CREATE 'toki-toki-ti '(mesa terremotos empanadas)
        (INSERT '(3 5 4) 'toki-toki-ti
                (INSERT '(2 2 4) 'toki-toki-ti
                        (INSERT '(1 2 3) 'toki-toki-ti
                                (FROM 'toki-toki-ti 'SELECT 'regs 'WHERE (= 'empanadas 4)))))))
;; caso típico
(define my-s-Cmd2 '(CREATE A (a b c) (INSERT (1 2 3) A (INSERT (4 5 6) A (FROM A SELECT regs WHERE (= a 1))))))
(define my-Cmd2 (CREATE 'A '(a b c) (INSERT '(1 2 3) 'A (INSERT '(4 5 6) 'A (FROM 'A 'SELECT 'regs 'WHERE (= 'a 1))))))

;; caso de borde: más de un CREATE
(define my-s-Cmd-nested-create '(CREATE A (a b c)
                                        (INSERT (1 2 3) A
                                                (INSERT (4 5 6) A
                                                        (CREATE B (d e f)
                                                                (INSERT (7 8 9) B
                                                                        (FROM A SELECT regs WHERE (= a 1))))))))
(define my-Cmd-nested-create (CREATE 'A '(a b c)
                                     (INSERT '(1 2 3) 'A
                                             (INSERT '(4 5 6) 'A
                                                     (CREATE 'B '(d e f)
                                                             (INSERT '(7 8 9) 'B
                                                                     (FROM 'A 'SELECT 'regs 'WHERE (= 'a 1))))))))

(define my-s-Cmd-nested-create2 '(CREATE A (a b c)
                                        (INSERT (1 2 3) A
                                                (INSERT (4 5 6) A
                                                        (CREATE B (d e f)
                                                                (INSERT (7 8 9) B
                                                                        (FROM B SELECT regs WHERE (= d 7))))))))

(define my-s-Cmd-nested-create3 '(CREATE A (a b c)
                                        (INSERT (1 2 3) A
                                                (INSERT (4 5 6) A
                                                        (CREATE B (d e f)
                                                                (INSERT (7 8 9) B
                                                                        (FROM A SELECT regs WHERE (= a 1))))))))
#| CASOS MAL FORMADOS |#

;; se inserta en una tabla inexistente
(define my-s-Cmd-ill-insertion '(CREATE A (a b c) (INSERT (1 2 3) B (INSERT (4 5 6) A (FROM A SELECT regs WHERE (= a 1))))))
(define my-s-Cmd-ill-insertion2 '(CREATE A (a b c)
                                        (INSERT (1 2 3) A
                                                (INSERT (4 5 6) A
                                                        (CREATE B (d e f)
                                                                (INSERT (7 8 9) C
                                                                        (FROM B SELECT regs WHERE (= d 7))))))))
;; se consulta una tabla inexistente
(define my-s-Cmd-ill-query '(CREATE A (a b c) (INSERT (1 2 3) A (INSERT (4 5 6) A (FROM B SELECT regs WHERE (= a 1))))))
(define my-s-Cmd-ill-query2 '(CREATE A (a b c)
                                        (INSERT (1 2 3) A
                                                (INSERT (4 5 6) A
                                                        (CREATE B (d e f)
                                                                (INSERT (7 8 9) B
                                                                        (FROM C SELECT regs WHERE (= d 7))))))))

;; se inserta un registro con una aridad inconsistente
(define my-s-Cmd-ill-arity '(CREATE A (a b c) (INSERT (1 2 3 4) A (INSERT (4 5 6) A (FROM A SELECT regs WHERE (= a 1))))))
(define my-s-Cmd-ill-arity2 '(CREATE A (a b c)
                                        (INSERT (1 2 3) A
                                                (INSERT (4 5 6) A
                                                        (CREATE B (d e f g)
                                                                (INSERT (7 8 9 10 11) B
                                                                        (FROM B SELECT regs WHERE (= d 7))))))))

;; se consulta una columna inexistente
(define my-s-Cmd-ill-column '(CREATE A (a b c) (INSERT (1 2 3) A (INSERT (4 5 6) A (FROM A SELECT regs WHERE (= d 1))))))
(define my-s-Cmd-ill-column2 '(CREATE A (a b c)
                                        (INSERT (1 2 3) A
                                                (INSERT (4 5 6) A
                                                        (CREATE B (d e f)
                                                                (INSERT (7 8 9) B
                                                                        (FROM B SELECT regs WHERE (= g 7))))))))
(define my-s-Cmd-ill-column3 '(CREATE A (a b c)
                                        (INSERT (1 2 3) A
                                                (INSERT (4 5 6) A
                                                        (CREATE B (d e f)
                                                                (INSERT (7 8 9) B
                                                                        (FROM B SELECT regs WHERE (= a 7))))))))
(define my-s-Cmd-ill-column4 '(CREATE A (a b c)
                                        (INSERT (1 2 3) A
                                                (INSERT (4 5 6) A
                                                        (CREATE B (d e f)
                                                                (INSERT (7 8 9) B
                                                                        (FROM A SELECT regs WHERE (= d 7))))))))

#| ==============================
            EJERCICIO 1
   ============================== |#

#| PARTE C |#

(test (parse my-s-Cmd) my-Cmd)
(test (parse my-s-Cmd2) my-Cmd2)
(test (parse my-s-Cmd-nested-create) my-Cmd-nested-create)

#| ==============================
            EJERCICIO 2
   ============================== |#

#| PARTE A |#

(test (check-table (parse my-s-Cmd)) #t)
(test (check-table (parse my-s-Cmd2)) #t)
(test (check-table (parse my-s-Cmd-nested-create)) #t)
(test (check-table (parse my-s-Cmd-nested-create2)) #t)
(test/exn (check-table (parse my-s-Cmd-ill-insertion)) "Error Estatico A1: Registro (1 2 3) insertado en tabla indefinida B")
(test/exn (check-table (parse my-s-Cmd-ill-insertion2)) "Error Estatico A1: Registro (7 8 9) insertado en tabla indefinida C")
(test/exn (check-table (parse my-s-Cmd-ill-query)) "Error Estatico A2: La tabla consultada B no se encuentra definida")
(test/exn (check-table (parse my-s-Cmd-ill-query2)) "Error Estatico A2: La tabla consultada C no se encuentra definida")

(test (exists-in-env? 'A (aEnv 'A 'dummy 'dummy (mtEnv))) #t)
(test (exists-in-env? 'B (aEnv 'A 'dummy 'dummy (mtEnv))) #f)

#| PARTE B |#

(test (check-arity (parse my-s-Cmd)) #t)
(test (check-arity (parse my-s-Cmd2)) #t)
(test (check-arity (parse my-s-Cmd-nested-create)) #t)
(test (check-arity (parse my-s-Cmd-nested-create2)) #t)
(test/exn (check-arity (parse my-s-Cmd-ill-arity)) "Error Estatico B: Registro (1 2 3 4) con aridad 4 insertado en tabla A de aridad 3")
(test/exn (check-arity (parse my-s-Cmd-ill-arity2)) "Error Estatico B: Registro (7 8 9 10 11) con aridad 5 insertado en tabla B de aridad 4")

(test (arity-match? '(1 2 3) 'A (aEnv 'dummy 'A '(a b c) (mtEnv))) #t)
(test (arity-match? '(1 2 3 4) 'A (aEnv 'dummy 'A '(a b c) (mtEnv))) #f)

#| PARTE C |#

(test (check-column (parse my-s-Cmd)) #t)
(test (check-column (parse my-s-Cmd2)) #t)
(test (check-column (parse my-s-Cmd-nested-create)) #t)
(test (check-column (parse my-s-Cmd-nested-create2)) #t)
(test/exn (check-column (parse my-s-Cmd-ill-column)) "Error Estático C: La columna d no está definida en la tabla A")
(test/exn (check-column (parse my-s-Cmd-ill-column2)) "Error Estático C: La columna g no está definida en la tabla B")
(test/exn (check-column (parse my-s-Cmd-ill-column3)) "Error Estático C: La columna a no está definida en la tabla B")
(test/exn (check-column (parse my-s-Cmd-ill-column4)) "Error Estático C: La columna d no está definida en la tabla A")

(test (check-cond-columns (= 'a 1) 'A (aEnv 'dummy 'A '(a b c) (mtEnv))) #t)
(test/exn (check-cond-columns (= 'd 1) 'A (aEnv 'dummy 'A '(a b c) (mtEnv))) "Error Estático C: La columna d no está definida en la tabla A")

(test (column-in-table? 'a 'A (aEnv 'dummy 'A '(a b c) (mtEnv))) #t)
(test (column-in-table? 'd 'A (aEnv 'dummy 'A '(a b c) (mtEnv))) #f) 

#| PARTE D |#

(test (static-check (parse my-s-Cmd)) #t)
(test (static-check (parse my-s-Cmd2)) #t)
(test (static-check (parse my-s-Cmd-nested-create)) #t)
(test (static-check (parse my-s-Cmd-nested-create2)) #t)
(test/exn (static-check (parse my-s-Cmd-ill-insertion)) "Error Estatico A1: Registro (1 2 3) insertado en tabla indefinida B")
(test/exn (static-check (parse my-s-Cmd-ill-insertion2)) "Error Estatico A1: Registro (7 8 9) insertado en tabla indefinida C")
(test/exn (static-check (parse my-s-Cmd-ill-query)) "Error Estatico A2: La tabla consultada B no se encuentra definida")
(test/exn (static-check (parse my-s-Cmd-ill-query2)) "Error Estatico A2: La tabla consultada C no se encuentra definida")
(test/exn (static-check (parse my-s-Cmd-ill-arity)) "Error Estatico B: Registro (1 2 3 4) con aridad 4 insertado en tabla A de aridad 3")
(test/exn (static-check (parse my-s-Cmd-ill-arity2)) "Error Estatico B: Registro (7 8 9 10 11) con aridad 5 insertado en tabla B de aridad 4")
(test/exn (static-check (parse my-s-Cmd-ill-column)) "Error Estático C: La columna d no está definida en la tabla A")
(test/exn (static-check (parse my-s-Cmd-ill-column2)) "Error Estático C: La columna g no está definida en la tabla B")
(test/exn (static-check (parse my-s-Cmd-ill-column3)) "Error Estático C: La columna a no está definida en la tabla B")
(test/exn (static-check (parse my-s-Cmd-ill-column4)) "Error Estático C: La columna d no está definida en la tabla A")

#| ==============================
            EJERCICIO 3
   ============================== |#

#| PARTE A |#



(test (interp-cmd (parse my-s-Cmd) empty-env) '((3 5 4) (2 2 4)))
(test (interp-cmd (parse my-s-Cmd2) empty-env) '((1 2 3)))
(test (interp-cmd (parse my-s-Cmd-nested-create2) empty-env) '((7 8 9)))
(test (interp-cmd (parse my-s-Cmd-nested-create3) empty-env) '((1 2 3)))

(test (interp-cond 'A (= 'a 1) (aEnv 'A 'reg '(1 2 3) (aEnv 'A 'column-names '(a b c) (mtEnv)))) '((1 2 3)))
(test (interp-cond 'A (= 'a 1) (aEnv 'A 'reg '(1 4 4) (aEnv 'A 'reg '(1 2 3) (aEnv 'A 'column-names '(a b c) (mtEnv))))) '((1 2 3) (1 4 4)))

(test (all-but-last '(1 2 3)) '(1 2))
(test (all-but-last '((1 2 3) (4 5 6))) '((1 2 3)))

(test (index-of '(1 2 3) 1) 0)

(test (env-lookup2 'A 'reg (aEnv 'A 'reg '(1 4 4) (aEnv 'A 'reg '(1 2 3) (aEnv 'A 'column-names '(a b c) (mtEnv))))) '((1 4 4) (1 2 3) (dummy)))

#| PARTE B |#

(test (run (parse my-s-Cmd)) '((3 5 4) (2 2 4)))
(test (run (parse my-s-Cmd2)) '((1 2 3)))
(test (run (parse my-s-Cmd-nested-create2)) '((7 8 9)))
(test (run (parse my-s-Cmd-nested-create3)) '((1 2 3)))
(test/exn (run (parse my-s-Cmd-ill-insertion)) "Error Estatico A1: Registro (1 2 3) insertado en tabla indefinida B")
(test/exn (run (parse my-s-Cmd-ill-insertion2)) "Error Estatico A1: Registro (7 8 9) insertado en tabla indefinida C")
(test/exn (run (parse my-s-Cmd-ill-query)) "Error Estatico A2: La tabla consultada B no se encuentra definida")
(test/exn (run (parse my-s-Cmd-ill-query2)) "Error Estatico A2: La tabla consultada C no se encuentra definida")
(test/exn (run (parse my-s-Cmd-ill-arity)) "Error Estatico B: Registro (1 2 3 4) con aridad 4 insertado en tabla A de aridad 3")
(test/exn (run (parse my-s-Cmd-ill-arity2)) "Error Estatico B: Registro (7 8 9 10 11) con aridad 5 insertado en tabla B de aridad 4")
(test/exn (run (parse my-s-Cmd-ill-column)) "Error Estático C: La columna d no está definida en la tabla A")
(test/exn (run (parse my-s-Cmd-ill-column2)) "Error Estático C: La columna g no está definida en la tabla B")