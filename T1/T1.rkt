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
;; Retorna la tarea con la duración más larga
(define (longest ts)
  (match ts
    [(task n v) (cons n v)]
    [(parallel-tasks l r) (if (> (cdr (longest l)) (cdr (longest r))) (longest l) (longest r))]
    [(serial-tasks l r) (if (> (cdr (longest l)) (cdr (longest r))) (longest l) (longest r))]))

#| PARTE E |#
;; sequest :: TaskSchedule -> integer
;; Retorna el largo de la secuencia más larga de tareas individuales
(define (sequest ts)
  (match ts
    [(task _ _) 1]
    [(parallel-tasks l r) (max (sequest l) (sequest r))]
    [(serial-tasks (parallel-tasks a b) (parallel-tasks c d)) (max (sequest (parallel-tasks a b)) (sequest (parallel-tasks c d)))]
    [(serial-tasks (parallel-tasks _ _) r) (sequest r)]
    [(serial-tasks l (parallel-tasks _ _)) (sequest l)]
    [(serial-tasks l r) (+ (sequest l) (sequest r))]))

#| PARTE F |#
;; end-time :: TaskSchedule string -> integer



#| PARTE G |#
;; Ayuda respecto a la firma: el tipo de retorno
;; del fold debe ser (TaskSchedule -> A)

(define (fold-taskschedule f g h)
  (λ (ts)
    (match ts
    [(task n v) (f ts)]
    [(parallel-tasks l r) (g 
                             ((fold-taskschedule f g h) l)
                             ((fold-taskschedule f g h) r))]
    [(serial-tasks l r)   (h 
                             ((fold-taskschedule f g h) l)
                             ((fold-taskschedule f g h) r))])))

 
#| PARTE H |#

(define (equal-n? n) (λ (x) (equal? (task-name x) n)))
(define (my-or? l r) (or l r))

(define (is-in2 n)
  (λ (ts)
   ((fold-taskschedule (equal-n? n) my-or? my-or?) ts)))

(define length2
   (fold-taskschedule task-length max +)) ;; eliminie el void y complete

(define (to-cons ts) (cons (task-name ts) (task-length ts)))
(define (max-cons l r) (if (> (cdr l) (cdr r)) l r))

(define longest2
  (fold-taskschedule to-cons max-cons max-cons)) ;; eliminie el void y complete