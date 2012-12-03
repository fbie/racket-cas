#lang racket

(require racket/future)
(require future-visualizer)
(require rackunit)
(require (planet dyoo/while-loop))

(require "../michael-scott-queue.rkt")

(define (enqueue-each n q)
  (enqueue n q)
  (when (not (equal? n 1)) (enqueue-each (sub1 n) q))
  )

(define (dequeue-each n q)
  (dequeue q)
  (when (not (equal? n 1)) (dequeue-each (sub1 n) q))
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

(define (run-n-futures n f)
  (if (> n 1)
    (cons (future f) (run-n-futures (sub1 n) f))
    (cons f null))
  )

(define (run-future-tests n f s q)
  (let ([chunk (/ s n)])
    (run-n-futures n (func-of-size f chunk q))
    )
  )

(define (block-run futures)
  (let [(me-touch-you (lambda (arg)
                        (if (future? arg)
                            (touch arg)
                            (arg))
                        ))]
    (for-each me-touch-you futures))
  )

(define (all-tests t s)
  (define q (make-msq))
  (visualize-futures (block-run (run-future-tests t enqueue-each s q)))
  (check-eq? (size q) s)
  ;(block-run (run-future-tests t dequeue-each s q))
  ;(check-eq? (size q) 0)
  ;(block-run (run-future-tests t enq-deq-each s q))
  ;(check-eq? (size q) 0)
  )

(define test-size 1000)
(all-tests 4 test-size)