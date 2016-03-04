#lang racket/base
(require racket/match syntax/location)
(provide read-password)

(module windows-utils racket/base
  (require ffi/unsafe)
  (provide getwch)
  (define MSVCRT (ffi-lib "msvcrt"))
  (define getwch/raw
    (get-ffi-obj "_getch" MSVCRT (_fun -> _int)
                 (lambda ()
                   (error 'msvcrt
                          "Cannot find msvcrt function \"_getch\""))))
  (define (getwch)
    (integer->char (getwch/raw))))

(module nix-utils racket/base
  (require ffi/unsafe)
  (provide getwch)
  (define LIBCURSES (ffi-lib "libcurses"))
  (define getwch/raw
    (get-ffi-obj "getch" LIBCURSES (_fun -> _int)
                 (lambda ()
                   (error 'libcurses
                          "Cannot find libcurses function \"getch\""))))
  (define (getwch)
    (integer->char (getwch/raw))))

(define windows-module-path (quote-module-path windows-utils))
(define     nix-module-path (quote-module-path     nix-utils))
(define getwch
  (if (eq? (system-type) 'windows)
      (dynamic-require windows-module-path 'getwch)
      (dynamic-require     nix-module-path 'getwch)))

(define (read-password)
  (define (return chars)
    (list->string (reverse chars)))
  (begin0 (let loop ((read-in null))
            (match (getwch)
              [(or #\return #\newline) (return read-in)]
              [#\003 (raise (raise exn:break loop))]
              [#\backspace (if (null? read-in) (loop read-in) (loop (cdr read-in)))]
              [other (loop (cons other read-in))]))
          (newline)))