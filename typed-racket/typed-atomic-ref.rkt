#lang typed/racket

(require/typed racket/future
               [make-fsemaphore (Integer -> FSemaphore)])

(define-type FSemaphore make-semaphore)

;(: atomic ((A) Semaphore FSemaphore A))
(struct atomic (sem fsem [ref #:mutable]))
;(struct: (A) atomic ([sem : Semaphore] [fsem : FSemaphore] [ref : A] ))
(define-type Atomic (atomic A))

(: make-atomic (All (A) -> (Atomic A)))
(define (make-atomic ref)
  (atomic (make-semaphore 1) (make-fsemaphore 1) ref)
  )

(: CAS (All (A) (Atomic A) A A -> Boolean))
(define (CAS ref exp new)
  (define (cas)
    (define success #t)
    (if (equal? (atomic-ref ref) exp)
      (set-atomic-ref! ref new)
      (set! success #f))
      success
    )
  (fsemaphore-wait (atomic-fsem ref))
  (let ([res (call-with-semaphore (atomic-sem ref) cas)])
    (fsemaphore-post (atomic-fsem ref))
    res)
  )

(provide make-atomic)
(provide atomic-ref)
(provide CAS)