#lang play
(require "T1.rkt")

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

(test (is-in my-taskschedule "t2") #t)
(test (is-in my-taskschedule "collect") #f)

(print-only-errors #t)


