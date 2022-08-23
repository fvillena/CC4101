#lang play
(require "T1.rkt")

(print-only-errors #t)

;; Definición del caso de prueba

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