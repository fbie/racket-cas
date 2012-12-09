#lang typed/racket

(require "typed-atomic-ref.rkt")

;; the node type of which the queue actually consists
(struct: node ([value : (U Void Any)] [next : (AtomicRef (U Void node))]))

(: len (All (A) (U node Void) -> Integer))
(define (len qn)
  (if (not (equal? qn (void)))
      (add1 (len (atomic-ref (node-next (cast qn node)))))
      0)
  )

;; a container type for the linked list
(struct: msq ([head : (AtomicRef node)] [tail : (AtomicRef node)]))

(: make-msq (-> msq))
(define (make-msq)
  (let ([head (node (void) (make-atomic (void)))])
    (msq (make-atomic head) (make-atomic head))
    )
  )

(: enqueue (Any msq -> Void))
(define (enqueue value q)
  (: try ( -> Void))
  (define (try)
    (let* [(node (node value (make-atomic (void))))
           (tail (msq-tail q))
           (next (atomic-ref (node-next (atomic-ref tail))))]
      (if (equal? tail (msq-tail q))
        (if (equal? next (void))
          (if (CAS (node-next (atomic-ref tail)) next node)
              (let ([a (CAS (msq-tail q) (atomic-ref tail) node)])
                (void))
              (let ([a (CAS (msq-tail q) (atomic-ref tail) next)])
                (try)))
          (try))
        (try))))
  (try)
  )

(: dequeue (msq -> Any))
(define (dequeue q)
  (: try (-> (U Void Any)))
  (define (try)
         (let* [(head (msq-head q))
                (tail (msq-tail q))
                (next (atomic-ref (node-next (atomic-ref head))))]
           (if (equal? head (msq-head q))
             (if (equal? (atomic-ref head) (atomic-ref tail))
                 (if (equal? next (void))
                     (void)
                     (let ([a (CAS (msq-tail q) (atomic-ref tail) next)])
                       (try)))
                 (if (CAS (msq-head q) (atomic-ref head) next)
                   (node-value (cast next node))
                   (try)))
             (try))))
  (try)
  )

(: size (msq -> Integer))
(define (size q)
  (len (atomic-ref (node-next (atomic-ref (msq-head q)))))
  )

(provide
 make-msq
 enqueue
 dequeue
 size)