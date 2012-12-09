#lang typed/racket
(require "typed-atomic-ref.rkt")

;; the node type of which the queue actually consists
(define-type Node node)
(struct: node ([value : (U Void Any)] [next : (AtomicRef (U Void Node))]))

(: len ((U Node Void) -> Integer))
(define (len qn)
  (if (not (equal? qn (void)))
      (add1 (len (atomic-ref (node-next (cast qn node)))))
      0)
  )

;; a container type for the linked list
(define-type MS-Queue msq)
(struct: msq ([head : (AtomicRef (U Void Node))] [tail : (AtomicRef (U Void Node))]))

(: make-msq (-> MS-Queue))
(define (make-msq)
  (let ([head (node (void) (make-atomic (void)))])
    (msq (make-atomic head) (make-atomic head))
    )
  )

(: enqueue (Any MS-Queue -> Void))
(define (enqueue value q)
  (: try ( -> Void))
  (define (try)
    (let* [(node (node value (make-atomic (void))))
           (tail (cast (atomic-ref (msq-tail q)) Node))
           (next (atomic-ref (node-next tail)))]
      (if (equal? tail (msq-tail q))
        (if (equal? next (void))
          (if (CAS (node-next tail) next node)
              (let ([a (CAS (msq-tail q) tail node)])
                (void))
              (let ([a (CAS (msq-tail q) tail next)])
                (try)))
          (try))
        (try))))
  (try)
  )

(: dequeue (MS-Queue -> Any))
(define (dequeue q)
  (: try (-> (U Void Any)))
  (define (try)
         (let* [(head (msq-head q))
                 (tail (msq-tail q))
                 (next (atomic-ref (node-next (cast (atomic-ref head) Node))))]
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

(: size (MS-Queue -> Integer))
(define (size q)
  (len (atomic-ref (node-next (cast (atomic-ref (msq-head q)) Node))))
  )

(provide
 make-msq
 enqueue
 dequeue
 size)