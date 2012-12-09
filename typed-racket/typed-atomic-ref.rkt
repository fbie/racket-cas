#lang typed/racket

(require/typed racket/future
               [#:opaque FSemaphore fsemaphore?]
               [make-fsemaphore (Integer -> FSemaphore)]
               [fsemaphore-wait (FSemaphore -> Void)]
               [fsemaphore-post (FSemaphore -> Void)])

(require/typed typed/racket
               [call-with-semaphore (All (A) Semaphore ( -> A) -> A)])

(struct: (T) atomic ([sem : Semaphore] [fsem : FSemaphore] [ref : T]) #:mutable)
(define-type (Atomic Semaphore FSemaphore T) (atomic T))
(define-type (AtomicRef T) (Atomic Semaphore FSemaphore T))

(: make-atomic (All (T) T -> (AtomicRef T)))
(define (make-atomic ref)
  (atomic (make-semaphore 1) (make-fsemaphore 1) ref)
  )

(: CAS (All (T) (AtomicRef (U Void T)) (U Void T) (U Void T) -> Boolean))
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
  (let ([res (call-with-semaphore (atomic-sem ref) cas)])
    (fsemaphore-post (atomic-fsem ref))
    res)
  )

(provide make-atomic)
(provide atomic-ref)
(provide CAS)
(provide AtomicRef)