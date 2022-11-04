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

;; --------- [ Parte 2 ] ---------

(test (propagate-consts (parse '{with {x 3} {with {y x} {+ x y}}})) (with 'y (num 3)(add (num 3)(id 'y))))
(test (propagate-consts (parse '{ if0 {+ 1 2} {+ 3 45} {- 4 6}})) ( if0 (add (num 1) (num 2)) (add (num 3) (num 45)) (sub (num 4) (num 6))))
(test (propagate-consts (parse '{{fun {x} x} 7})) (app (fun 'x (id 'x)) (num 7)))
;; (test (propagate-consts (parse '{with {x 23} {fun {y} x}})) (fun 'y (num 23)))
;; (test (propagate-consts (parse '{fun {y} {with {x 7} {+ x x}}})) (fun 'y (with 'x (num 7) (add (id 'x) (id 'x)))))

;; (test (propagate-consts (parse '{with {x y} {with {y x}{+ x y}}})) (parse '{with {x y} {with {y x}{+ x y}}}))
;; (test (propagate-consts (fun 'x (with 'y (num 7) (app (fun 'z (add (id 'y) (id 'z))) (num 3))))) (fun 'x (with 'y (num 7) (app (fun 'z (add (id 'y) (id 'z))) (num 3)))))

;; --------- [ Parte 3 ] ---------

(test (cf&p (parse '{with {x {+ 17 8}} {+ x 4}})) (num 29))
(test (cf&p (parse '{ if0 {+ 1 2} {+ 3 15} {- 4 6}})) (num -2))
;; (test (cf&p (parse '{with {x {+ 6 7}} {with {y {+ x 17}} {with {z {+ y 9}} {+ 3 z}}}})) (num 42))
(test (cf&p (parse '{{fun {x} x} 7})) (app (fun 'x (id 'x)) (num 7)))
(test (cf&p (parse '{with {x {+ 2 {{fun {y} y} 10}}} {+ x 8}})) (with 'x (add (num 2) (app (fun 'y (id 'y)) (num 10))) (add (id 'x) (num 8))))