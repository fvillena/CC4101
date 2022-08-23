#lang play


#| PARTE A |#

#|
<TaskSchedule> ::=  (tarea <string> <number>)
                 |  (parallel-tasks <TaskSchedule> <TaskSchedule>)
                 |  (serial-tasks <TaskSchedule> <TaskSchedule>)
|#

(deftype TaskSchedule
  (task name length)
  (parallel-tasks tl tr)
  (serial-tasks tl tr))

#| PARTE B |#
;; is-in :: TaskSchedule string -> bool




#| PARTE C |#
;; length :: TaskSchedule -> integer



#| PARTE D |#
;; width :: TaskSchedule -> integer



#| PARTE E |#
;; longest :: TaskSchedule -> cons string integer



#| PARTE F |#
;; end-time :: TaskSchedule string -> integer



#| PARTE G |#
;; Ayuda respecto a la firma: el tipo de retorno
;; del fold debe ser (TaskSchedule -> A)

 
#| PARTE H |#
(define is-in2
  void) ;; eliminie el void y complete

(define length2
   void) ;; eliminie el void y complete

(define longest2
   void) ;; eliminie el void y complete
