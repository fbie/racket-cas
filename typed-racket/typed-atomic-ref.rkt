#lang typed/racket

(require/typed racket/future
               [#:opaque FSemaphore fsemaphore?]
               [make-fsemaphore (Integer -> FSemaphore)]
               [fsemaphore-wait (FSemaphore -> Void)]
               [fsemaphore-post (FSemaphore -> Void)])

(struct: (A) atomic ([sem : Semaphore] [fsem : FSemaphore] [ref : A]) #:mutable)
(define-type (Atomic Semaphore FSemaphore A) (atomic A))
(define-type (AtomicRef A) (Atomic Semaphore FSemaphore A))

(: make-atomic (All (A) A -> (AtomicRef A)))
(define (make-atomic ref)
  (atomic (make-semaphore 1) (make-fsemaphore 1) ref)
  )

(: CAS (All (A) (AtomicRef (U Void A)) (U Void A) (U Void A) -> Boolean))
(define (CAS ref exp new)
  (: cas ( -> Boolean))
  (define (cas)
    (define success #t)
    (if (equal? (atomic-ref ref) exp)
      (set-atomic-ref! ref new)
      (set! success #f))
      success
    )
  (fsemaphore-wait (atomic-fsem ref))
  (semaphore-wait (atomic-sem ref))
  (let ([res (cas)])
    (semaphore-post (atomic-sem ref))
    (fsemaphore-post (atomic-fsem ref))
    res)
  )

(provide make-atomic)
(provide atomic-ref)
(provide CAS)
(provide AtomicRef)