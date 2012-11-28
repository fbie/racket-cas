#lang racket

;; the empty beginning of the queue
(struct bgn ())

;; enq is trivial, as it is simply a
;; constructor extending an existing queue
(struct enq (val tail))

(define
  (get-first queue)
  (match queue
    [(bgn) (#f)]
    [(enq val tail)
     (match tail
       [(bgn) val]
       [(enq _ _) (get-first tail)])]
    )
  )

(define
  (deq queue)
  (match queue
    [(bgn) (#f)]
    [(enq val tail)
     (match tail
       [(bgn) tail] ;; remove the first item in queue
       [(enq _ _) (enq val (deq tail))])]
    )
  )
