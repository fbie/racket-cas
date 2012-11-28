#lang racket

(struct atomic (sem ref))

(define (make-atomic ref)
  (atomic (make-semaphore 1) ref)
  )

(define (CAS ref exp new)
  (define 
    (cas)
    (if (equal? (atomic-ref ref) exp)
      (atomic (atomic-sem ref) new)
      #f
      )
    )
  (call-with-semaphore (atomic-sem ref) cas)
  )
