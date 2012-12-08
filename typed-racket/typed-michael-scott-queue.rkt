#lang typed/racket

(require (planet dyoo/while-loop))
(require "typed-atomic-ref.rkt")

;; the node type of which the queue actually consists
;(Rec BT (U Number (Pair BT BT))))
(define-type (Node A) (Rec (Next A) (A (Next A))))
(struct: (A) node ([value : A] next))

(: len (All (A) (Node A) -> Integer))
(define (len qn)
  (if (not (equal? qn void))
      (add1 (len (atomic-ref (node-next qn))))
      0)
  )

;; a container type for the linked list
(struct: (A) msq ([head : (Node A)] [tail : (Node A)]))
(define-type (MSQ A) ((Node A) (Node A)))

(: make-msq (All (A) -> (MSQ A)))
(define (make-msq)
  (let ([head (node void (make-atomic void))])
    (msq (make-atomic head) (make-atomic head))
    )
  )

(: enqueue (All (A) A (MSQ A) -> Void))
(define (enqueue value q)
  (while #t
         (let* [(node (node value (make-atomic void)))
                (tail (msq-tail q))
                (next (atomic-ref (node-next (atomic-ref tail))))]
           (when (equal? tail (msq-tail q))
             (when (equal? next void)
               (if (CAS (node-next (atomic-ref tail)) next node)
                   ((CAS (msq-tail q) (atomic-ref tail) node)
                    (break))
                   (CAS (msq-tail q) tail next))))))
  (void)
  )

(: dequeue (All (A) (MSQ A) -> (U Void A)))
(define (dequeue q)
  (define return void)
  (while #t
         (let* [(head (msq-head q))
                (tail (msq-tail q))
                (next (atomic-ref (node-next (atomic-ref head))))]
           (when (equal? head (msq-head q))
             (if (equal? (atomic-ref head) (atomic-ref tail))
                 (if (equal? next void)
                     (break)
                     (CAS (msq-tail q) tail next))
                 (when (CAS (msq-head q) (atomic-ref head) next)
                   ((set! return (node-value next))
                    (break))
                   )))))
  return
  )
  
(: size (All (A) (MSQ A) -> Integer))
(define (size q)
  (len (atomic-ref (node-next (atomic-ref (msq-head q)))))
  )

(provide make-msq)
(provide enqueue)
(provide dequeue)
(provide size)