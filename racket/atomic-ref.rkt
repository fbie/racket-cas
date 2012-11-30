#lang racket

(struct atomic (sem [ref #:mutable]))

(define (make-atomic ref)
  (atomic (make-semaphore 1) ref)
  )

(define (CAS ref exp new)
  (define (cas)
    (if (equal? (atomic-ref ref) exp)
      [and (set-atomic-ref! ref new) #t]
      #f
      )
    )
  (call-with-semaphore (atomic-sem ref) cas)
  )
