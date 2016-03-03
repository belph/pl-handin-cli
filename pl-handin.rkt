#lang racket/base
(require "client-cli.rkt" racket/cmdline)
(define username   (make-parameter #f))
(define assignment (make-parameter #f))
(define file       (make-parameter #f))

(define pl-args
  (command-line
   #:once-each
   [("-u" "--username")   user "Handin Server Username"
                          (username user)]
   [("-a" "--assignment") assign "Assignment to submit"
                          (assignment assign)]
   #:args ([submit-file #f])
   (file submit-file)
   `((username   . ,(username))
     (assignment . ,(assignment))
     (to-submit  . ,(file)))))

(pl-submit pl-args)