#lang racket

(require (planet dyoo/while-loop))
(require "atomic-ref.rkt")

;; the node type of which the queue actually consists
(struct node (value [next #:mutable]))

(define (len qn)
  (if (not (equal? qn void))
      (add1 (len (atomic-ref (node-next qn))))
      0)
  )
  

;; a container type for the linked list
(struct msq (head [tail #:mutable]))

(define (make-msq)
  (let ([head (node void (make-atomic void))])
    (msq (make-atomic head) (make-atomic head))
    )
  )

(define (enqueue value q)
  (while #t
         (let* [(node (node value (make-atomic void)))
                (tail (msq-tail q))
                (next (atomic-ref (node-next (atomic-ref tail))))]
           (when (equal? tail (msq-tail q))
             (when (equal? next void)
               (if (CAS (node-next (atomic-ref tail)) next node)
                   ((CAS (msq-tail q) (atomic-ref tail) node) ;; moved from the end of the function to here for convenience
                    (break))
                   (CAS (msq-tail q) tail next))))))
  )

(define (dequeue q)
  (while #t
         (let* [(head (msq-head q))
                (tail (msq-tail q))
                (next (atomic-ref (node-next (atomic-ref head))))]
           (when (equal? head (msq-head q))
             (if (equal? (atomic-ref head) (atomic-ref tail))
                 (if (equal? next void)
                     (break) ;; how to return #f?
                     (CAS (msq-tail q) tail next))
                 (when (CAS (msq-head q) (atomic-ref head) next)
                   (break)))))) ;; how to return #t?
  )
  
(define (size q)
  (len (atomic-ref (node-next (atomic-ref (msq-head q)))))
  )