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
