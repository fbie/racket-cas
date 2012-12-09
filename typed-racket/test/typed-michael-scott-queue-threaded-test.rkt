#lang racket

(require rackunit)
(require (planet dyoo/while-loop))

(require "../typed-michael-scott-queue.rkt")

(define (enqueue-each n q)
  (enqueue n q)
  (when (> n 1) (enqueue-each (sub1 n) q))
  )

(define (dequeue-each n q)
  (dequeue q)
  (when (> n 1) (dequeue-each (sub1 n) q))
  )

(define (enq-deq-each n q)
  (let [(n1 n)]
    (enqueue-each n1 q))
  (let [(n2 n)]
    (dequeue-each n2 q))
  )

(define (func-of-size f s q)
  (define (func)
    (f s q))
  func
  )

;; test traditional threading
(define (run-n-threads n f)
  (if (> n 0)
      (cons (thread f) (run-n-threads (sub1 n) f))
      null)
  )

(define (run-thread-tests n f s q)
  (let ([chunk (/ s n)])
    (run-n-threads n (func-of-size f chunk q))
    )
  )

(define (block-run threads)
  (when (not (null? threads))
    (block-run (cdr threads))
    (while (not (thread-dead? (car threads)))
           void))
  )
  
(define (all-tests t s)
  (define q (make-msq))
  (block-run (run-thread-tests t enqueue-each s q))
  (check-eq? (size q) test-size)
  (block-run (run-thread-tests t dequeue-each s q))
  (check-eq? (size q) 0)
  (block-run (run-thread-tests t enq-deq-each s q))
  (check-eq? (size q) 0)
  )

(define test-size 1000000)
(all-tests 4 test-size)