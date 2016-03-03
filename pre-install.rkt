#lang racket
(provide pre-installer)

(define (assert-installed module-path)
  (with-handlers ([exn:fail? (λ(e) (raise-user-error (format "Missing dependency: ~a" module-path)))])
    ((current-module-name-resolver) module-path #f #f #f)
    (void)))

(define (pre-installer racket-path)
  ;; Make sure pl is installed before installing
  ;; (Check thoroughly)
  (display "pre-install: Checking that pl is installed...")
  (assert-installed 'pl/info)
  (assert-installed 'pl/client)
  (displayln "done."))