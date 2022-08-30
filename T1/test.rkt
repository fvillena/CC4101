#lang play
(require "T1.rkt")

(print-only-errors #t)

#| PARTE A |#
;; Definición del caso de prueba de la tarea

(define my-taskschedule
  (serial-tasks
   (parallel-tasks
    (serial-tasks
     (task "t1" 2)
     (task "t2" 4))
    (task "t3" 3))
   (parallel-tasks
    (serial-tasks
     (task "t4" 2)
     (parallel-tasks
      (task "t6" 2)
      (task "t7" 1)))
    (task "t5" 6))))

#| PARTE B |#

(test (is-in (task "t" 3) "t") #t)
(test (is-in (task "t" 3) "u") #f)
(test (is-in (parallel-tasks (task "t0" 4) (task "t1" 2)) "t0") #t)
(test (is-in (parallel-tasks (task "t0" 4) (task "t1" 2)) "u0") #f)
(test (is-in (parallel-tasks (serial-tasks (task "t0" 1) (serial-tasks (task "t1" 1) (task "t2" 1))) (serial-tasks (task "t3" 1) (task "t4" 1)))  "t2") #t)
(test (is-in my-taskschedule "t2") #t)
(test (is-in my-taskschedule "collect") #f)

#| PARTE C |#

(test (length (task "t" 3)) 3)
(test (length (parallel-tasks (task "t0" 4) (task "t1" 2))) 4)
(test (length (serial-tasks (task "t0" 4) (task "t1" 2))) 6)
(test (length my-taskschedule) 12)

#| PARTE D |#

(test (longest (task "t" 3)) (cons "t" 3))
(test (longest (parallel-tasks (task "t0" 4) (task "t1" 2))) (cons "t0" 4))
(test (longest (serial-tasks (task "t0" 4) (task "t1" 2))) (cons "t0" 4))
(check-true (or; Caso de borde cuando hay 2 tareas del mismo largo pero distinto nombre se puede devolver cualquiera
             (equal? (longest (serial-tasks (task "t0" 4) (task "t1" 4))) (cons "t0" 4))
             (equal? (longest (serial-tasks (task "t0" 4) (task "t1" 4))) (cons "t1" 4))))
(test (longest my-taskschedule) (cons "t5" 6))

#| PARTE E |#

(test (sequest (task "t" 3)) 1)

(test (sequest (parallel-tasks (task "t0" 4) (task "t1" 2))) 1)

(test (sequest (parallel-tasks (serial-tasks (task "t0" 4) (task "t1" 2)) (task "t1" 2))) 2)
(test (sequest (parallel-tasks (serial-tasks (task "t0" 4) (task "t1" 2)) (serial-tasks (task "t0" 4) (serial-tasks (task "t0" 4) (task "t1" 2))))) 3)

(test (sequest (serial-tasks (task "t0" 4) (parallel-tasks (task "t1" 2) (task "t1" 2)))) 1)
(test (sequest (serial-tasks (parallel-tasks (task "t1" 2) (task "t1" 2)) (task "t0" 4))) 1)
(test (sequest (serial-tasks (parallel-tasks (task "t1" 2) (task "t1" 2)) (parallel-tasks (task "t1" 2) (serial-tasks (task "t0" 4) (task "t1" 2))))) 2)

(test (sequest (serial-tasks (task "t0" 4) (task "t1" 2))) 2)
(test (sequest (serial-tasks (task "t0" 4) (serial-tasks (task "t0" 4) (task "t1" 2)))) 3)
(test (sequest my-taskschedule) 2)

#| PARTE F |#

(test (end-time (task "t" 3) "t") 3)
(test/exn (end-time (task "t" 3) "u") "no encontrado")
(test (end-time (serial-tasks (task "t0" 1) (task "t1" 1)) "t1") 2)
(test (end-time (serial-tasks (task "t1" 1) (task "t0" 1)) "t1") 1)
(test (end-time (parallel-tasks (task "t0" 1) (task "t1" 1)) "t1") 1)
(test (end-time (parallel-tasks (task "t1" 1) (task "t0" 1)) "t1") 1)
(test (end-time (serial-tasks (task "t0" 1) (serial-tasks (task "t1" 1) (task "t2" 1))) "t1") 2)
(test (end-time (serial-tasks (task "t0" 1) (serial-tasks (task "t1" 1) (task "t2" 1))) "t2") 3)
(test (end-time (parallel-tasks (serial-tasks (task "t0" 1) (serial-tasks (task "t1" 1) (task "t2" 1))) (serial-tasks (task "t3" 1) (task "t4" 1)))  "t2") 3)
(test (end-time (parallel-tasks (serial-tasks (task "t0" 1) (serial-tasks (task "t1" 1) (task "t2" 1))) (serial-tasks (task "t3" 1) (task "t4" 1)))  "t4") 2)
(test (end-time my-taskschedule "t7") 9)

#| PARTE H |#

(test ((is-in2 "t") (task "t" 3)) #t)
(test ((is-in2 "u") (task "t" 3)) #f)
(test ((is-in2 "t0") (parallel-tasks (task "t0" 4) (task "t1" 2))) #t)
(test ((is-in2 "u0") (parallel-tasks (task "t0" 4) (task "t1" 2))) #f)
(test ((is-in2 "t2") my-taskschedule) #t)
(test ((is-in2 "collect") my-taskschedule) #f)

(test (length2 (task "t" 3)) 3)
(test (length2 (parallel-tasks (task "t0" 4) (task "t1" 2))) 4)
(test (length2 (serial-tasks (task "t0" 4) (task "t1" 2))) 6)
(test (length2 my-taskschedule) 12)

(test (longest2 (task "t" 3)) (cons "t" 3))
(test (longest2 (parallel-tasks (task "t0" 4) (task "t1" 2))) (cons "t0" 4))
(test (longest2 (serial-tasks (task "t0" 4) (task "t1" 2))) (cons "t0" 4))
(test (longest2 my-taskschedule) (cons "t5" 6))