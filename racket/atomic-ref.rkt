#lang racket

(require racket/future)

(struct atomic (sem fsem [ref #:mutable]))

(define (make-atomic ref)
  (atomic (make-semaphore 1) (make-fsemaphore 1) ref)
  )

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

;;(provide
;; (contract-out
;;  [struct atomic ((sem make-semaphore)
;;                  (ref any/c))]
;;  [make-atomic (-> any/c atomic)]
;;  [CAS (-> atomic any/c any/c)]))