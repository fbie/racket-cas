#lang racket

(require (planet dyoo/while-loop))
(require "atomic-ref.rkt")

;; the node type of which the queue actually consists
(struct ms-qnode (value [next #:mutable]))

(define (len qn)
  (if (not (equal? qn void))
      (add1 (len (atomic-ref (ms-qnode-next qn))))
      0)
  )
  

;; a container type for the linked list
(struct ms-queue (head [tail #:mutable]))

(define (make-ms-queue)
  (let ([head (ms-qnode void (make-atomic void))])
    (ms-queue (make-atomic head) (make-atomic head))
    )
  )

(define (enqueue value q)
  (while #t
         (let* [(node (ms-qnode value (make-atomic void)))
                (tail (ms-queue-tail q))
                (next (atomic-ref (ms-qnode-next (atomic-ref tail))))]
           (when (equal? tail (ms-queue-tail q))
             (when (equal? next void)
               (if (CAS (ms-qnode-next (atomic-ref tail)) next node)
                   ((CAS (ms-queue-tail q) (atomic-ref tail) node) ;; moved from the end of the function to here for convenience
                    (break))
                   (CAS (ms-queue-tail q) tail next))))))
  )

(define (dequeue q)
  (while #t
         (let* [(head (ms-queue-head q))
                (tail (ms-queue-tail q))
                (next (atomic-ref (ms-qnode-next (atomic-ref head))))]
           (when (equal? head (ms-queue-head q))
             (if (equal? (atomic-ref head) (atomic-ref tail))
                 (if (equal? next void)
                     (break) ;; how to return #f?
                     (CAS (ms-queue-tail q) tail next))
                 (when (CAS (ms-queue-head q) (atomic-ref head) next)
                   (break)))))) ;; how to return #t?
  )
  
(define (size q)
  (len (atomic-ref (ms-qnode-next (atomic-ref (ms-queue-head q)))))
  )
