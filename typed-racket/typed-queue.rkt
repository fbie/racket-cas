#lang typed/racket

;; the node type of which the queue actually consists
(define-type QNode (qnode))
(struct: (A) qnode ([value : A] [[next : QNode] : #:mutable]))

(define (find-end qn)
  (let ([next (qnode-next qn)])
    (if next
        [find-end next]
        qn)
    )
  )

(define (enq val qn)
  (let ([end (find-end qn)])
    (set-qnode-next! end (qnode val #f))
    )
  )

(define (len qn)
  (if (not (equal? qn void))
      (add1 (len (qnode-next qn)))
      0)
  )

;; a container type for the linked list
(struct queue (head [tail #:mutable]))

(define (make-queue)
  (let ([head (qnode void void)])
    (queue head head)
    )
  )

(define (enqueue value q)
  (let ([new (qnode value void)])
    (set-qnode-next! (queue-tail q) new)
    (set-queue-tail! q new)
    )
  )

(define (dequeue q)
  (when (not (equal? (queue-head q) (queue-tail q)))
    (let ([head (queue-head q)])
      (set-qnode-next! head (qnode-next (qnode-next head)))
      )
    )
  )

(define (size q)
  (len (qnode-next (queue-head q)))
  )

