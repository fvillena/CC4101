#lang play


#| PARTE A |#

#|
<TaskSchedule> ::=  (tarea <string> <number>)
                 |  (parallel-tasks <TaskSchedule> <TaskSchedule>)
                 |  (serial-tasks <TaskSchedule> <TaskSchedule>)
|#

;; Tipo inductivo para representar Task Schedules
(deftype TaskSchedule
  (task name length)
  (parallel-tasks tl tr)
  (serial-tasks tl tr))

#| PARTE B |#
;; is-in :: TaskSchedule string -> bool
;; Retorna si una tarea con nombre n está presente en el Task Schedule
(define (is-in ts n)
  (match ts
    [(task v _) (equal? v n)]
    [(parallel-tasks l r) (or (is-in l n)
                              (is-in r n))]
    [(serial-tasks l r) (or (is-in l n)
                            (is-in r n))]))

#| PARTE C |#
;; length :: TaskSchedule -> integer
;; Retorna la duración total de un Task Schedule
(define (length ts)
  (match ts
    [(task _ v) v]
    [(parallel-tasks l r) (max (length l) (length r))]
    [(serial-tasks l r) (+ (length l) (length r))]))

#| PARTE D |#
;; longest :: TaskSchedule -> cons string integer

(define (longest ts)
  (match ts
    [(task _ v) v]
    [(parallel-tasks l r) (max (longest l) (longest r))]
    [(serial-tasks l r) (max (longest l) (longest r))]))

#| PARTE E |#
;; sequest :: TaskSchedule -> integer



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