#lang racket

(require "typed-atomic-ref.rkt")

;; the node type of which the queue actually consists
(struct node (value next))

(define (len qn)
  (if (not (equal? qn (void)))
      (add1 (len (atomic-ref (node-next qn))))
      0)
  )

;; a container type for the linked list
(struct msq (head tail))

(define (make-msq)
  (let ([head (node (void) (make-atomic (void)))])
    (msq (make-atomic head) (make-atomic head))
    )
  )

(define (enqueue value q)
  (define (try)
    (let* [(node (node value (make-atomic (void))))
           (tail (msq-tail q))
           (next (atomic-ref (node-next (atomic-ref tail))))]
      (if (equal? tail (msq-tail q))
        (if (equal? next (void))
          (if (CAS (node-next (atomic-ref tail)) next node)
              (let ([a (CAS (msq-tail q) (atomic-ref tail) node)])
                (void))
              (let ([a (CAS (msq-tail q) tail next)])
                (try)))
          (try))
        (try))))
  (try)
  )

(define (dequeue q)
  (define (try)
         (let* [(head (msq-head q))
                (tail (msq-tail q))
                (next (atomic-ref (node-next (atomic-ref head))))]
           (if (equal? head (msq-head q))
             (if (equal? (atomic-ref head) (atomic-ref tail))
                 (if (equal? next (void))
                     (void)
                     (let ([a (CAS (msq-tail q) tail next)])
                       (try)))
                 (if (CAS (msq-head q) (atomic-ref head) next)
                   (node-value next)
                   (try)))
             (try))))
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