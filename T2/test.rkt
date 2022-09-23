#lang play
(require "T2.rkt")

;;(CREATE toki-toki-ti (mesa terremotos empanadas)
;;        (INSERT (3 5 4) toki-toki-ti
;;                (INSERT (2 2 4) toki-toki-ti
;;                        (INSERT (1 2 3) toki-toki-ti
;;                                (FROM toki-toki-ti SELECT regs WHERE (= empanadas 4))))))


(define my-s-Cmd '(CREATE A (a b c) (INSERT (1 2 3) A (INSERT (4 5 6) A (FROM A SELECT regs WHERE (= a 1))))))

(parse my-s-Cmd)

(print-only-errors #t)


