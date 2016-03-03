#lang racket
(require setup/dirs (only-in "client-cli.rkt" tty?))

(provide post-installer)

(define fancy-string "\033[31;1mNOTE:\033[0m\033[32m To use, you should add ~a to your PATH.\033[39;49m\n")
(define basic-string "NOTE: To use, you should add ~a to your PATH.\n")
(define show-fancy? (and tty? (not (eq? (system-type) 'windows))))

(define (post-installer racket-path)
  (displayln "Installation Successful.")
  (printf (if show-fancy?
              fancy-string
              basic-string)
          (path->string (find-user-console-bin-dir))))