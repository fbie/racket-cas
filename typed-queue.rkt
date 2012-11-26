#lang typed/racket

(define-type (Queue A) (U bgn (enq A)))

;; the empty beginning of the queue
(struct: bgn ())

;; enq is trivial, as it is simply a
;; constructor extending an existing queue
(struct: (A) enq ([val : A] [tail : (Queue A)]))

(: get-first (All (A) (Queue A) -> (U A #f)))
(define
  (get-first queue)
  (match queue
    [(bgn) #f]
    [(enq val tail)
     (match tail
       [(bgn) val]
       [(enq _ _) (get-first tail)])]
    )
  )

(: deq (All (A) (Queue A) -> (Queue A)))
(define
  (deq queue)
  (match queue
    [(bgn) (bgn)] ;; nothing to remove
    [(enq val tail)
     (match tail
       [(bgn) tail] ;; remove the first item in queue
       [(enq _ _) (enq val (deq tail))])]
    )
  )