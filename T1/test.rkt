#lang play
(require "T1.rkt")

;; Definición

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

(print-only-errors #t)


