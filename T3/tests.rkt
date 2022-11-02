#lang play
(require "T3.rkt")

(print-only-errors #t)

;; --------- [ Parte 1 ] ---------

(test (const? (parse '{ if0 {+ 1 2} {+ 3 45} {- 4 6}})) #t)
(test (const? (parse '{with {x 18} 3})) #t)

(test (const? (parse '{{fun {x} x} 19})) #f)
(test (const? (parse '{fun {x} 10})) #f)
(test (const? (parse '{with {x 5} {+ 1 x}})) #f)

(test ( fold-consts (parse '{ if0 {+ 1 2} {+ 3 45} {- 4 6}})) (num -2))
(test ( fold-consts (parse '{with {x {+ 2 3}} {+ 1 x}})) (with 'x (num 5) (add (num 1) (id 'x))))
(test ( fold-consts (parse '{{fun {x} {+ 2 3}} 7})) (app (fun 'x (add (num 2) (num 3))) (num 7)))