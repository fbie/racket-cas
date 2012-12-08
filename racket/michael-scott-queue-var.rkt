#lang racket

(require "atomic-ref.rkt")

;; the node type of which the queue actually consists
(struct node (value next))

(define (len qn)
  (if (not (equal? qn (void)))
      (add1 (len (atomic-ref (node-next qn))))
      0)
  )
  
;; a container type for the linked list
(struct msq (head))

(define (make-msq)
  (let ([head (node (void) (make-atomic (void)))])
    (msq (make-atomic head))
    )
  )

(define (enqueue value q)
  (let ([n (node value (make-atomic (void)))])
    (define (try c)
      (if (equal? (atomic-ref (node-next c)) (void))
          (when (not (CAS (node-next c) (void) n))
              (try c))
          (try (atomic-ref (node-next c))))
      )
      (try (atomic-ref (msq-head q))))
  )

(define (dequeue q)
  (define (try)
    (let ([n (atomic-ref (msq-head q))])
      (if (equal? (atomic-ref (node-next n)) void)
          void
          (if (CAS (msq-head q) n (atomic-ref (node-next n)))
              (node-value (atomic-ref (node-next n)))
              (try)))))
  (try)
  )
  
(define (size q)
  (len (atomic-ref (node-next (atomic-ref (msq-head q)))))
  )

(provide
 (contract-out
  [make-msq (-> msq?)]
  [enqueue (-> any/c msq? void?)]
  [dequeue (-> msq? (or/c any/c void?))]
  [size (-> msq? integer?)]))