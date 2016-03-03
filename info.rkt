#lang info
(define collection "pl-handin-cli")
(define version "1.0")

(define racket-launcher-names (list "pl-handin" "pl-retrieve"))
(define racket-launcher-libraries (list "pl-handin.rkt" "pl-retrieve.rkt"))

(define  pre-install-collection "pre-install.rkt")
(define post-install-collection "post-install.rkt")