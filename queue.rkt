#lang racket

;; the empty begin of the queue
(struct bgn ())

;; enq is trivial, as it is simply a
;; constructor extending an existing queue
(struct enq (val tail))

(define
  (get-first queue)
  (match queue
    [(enq val tail)
     (match tail
       [(bgn) val]
       [(enq _ _) (get-first tail)])]
    [(bgn) (#f)]
    )
  )

(define
  (deq queue)
  (match queue
    [(enq val tail)
     (match tail
       [(bgn) tail] ;; remove the first item in queue
       [(enq _ _) (enq val (deq tail))])]
    [(bgn) (#f)]
    )
  )