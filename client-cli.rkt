#lang racket/base
(require pl/info pl/client
         net/sendurl racket/file racket/list wxme racket/gui
         racket/class racket/exn "term-utils.rkt")

(provide (rename-out [init-and-submit   pl-submit]
                     [init-and-retrieve pl-retrieve])
         tty?)

(define (with-format func #:default-port (dport (current-output-port)))
  (lambda (str #:port (port dport) . args)
    (func (apply format str args) port)))

(define displayln/f (with-format displayln))
(define   display/f (with-format display  ))
(define (displayln/err s) (displayln s (current-error-port)))
(define displayln/err/f (with-format displayln/err))

(define remembered-assignment (make-parameter #f))
(define uninstalled? #f)
(define comm-cust #f)
(define committing? #f)
(define commit-lock #f)
(define go-sema #f)
(define connection #f)
(define login-sema #f)

(define assignments null)
(define default-assignment #f)

(define server:port (#%info-lookup 'server:port (lambda () #f)))
(define-values (server port-no)
  (if server:port
      (let ([m (regexp-match #rx"^([^:]+):([0-9]+)$" server:port)])
        (unless m
          (error 'handin-client
                 "Bad configuration ~s, expecting \"server:port\""
                 server:port))
        (values (cadr m) (string->number (caddr m))))
      (values #f #f)))

(define handin-name   (#%info-lookup 'name))
(define web-menu-name (#%info-lookup 'web-menu-name (lambda () #f)))
(define web-address   (#%info-lookup 'web-address   (lambda () #f)))

(define (connect) (handin-connect server port-no))

;; Like (read), but makes sure to consume a line of input
;; (if the line is empty, returns #f)
(define (read/read-line #:reader (reader read-line))
  (let ([line (reader)])
    (and (not (equal? eof line))
         (> (string-length line) 0)
         (read (open-input-string line)))))

;; Displays a prompt to (current-output-port) for
;; the given list of options (input is received from
;; (current-input-port))
(define (prompt options #:text (prompt-text #f) #:default (default #f))
  (define default-text
    (if default
        (format " (default: ~a)" default)
        ""))
  (define (input->option i)
    (if (and (number? i) (<= 1 i (length options)))
        (list-ref options (sub1 i))
        (let ((search-result (member i options)))
          (and search-result
               (car search-result)))))
  (when prompt-text
    (displayln prompt-text))
  (for ((idx (in-naturals 1))
        (option options))
       (displayln/f "[~a] ~a" idx option))
  (display/f "Enter choice~a: " default-text)
  (let loop ([input (read/read-line)])
    (cond [(and (not input) (not default))
           (display/f "Enter choice~a: " default-text)
           (loop (read/read-line))]
          [(not input) default]
          [else (let ([as-option (input->option input)])
                  (or as-option
                      (begin
                        (display/f "Invalid option: ~a~nEnter choice: " input)
                        (loop (read/read-line)))))])))

;; Displays a prompt to (current-output-port) for
;; a yes/no answer (input is received from
;; (current-input-port))
(define (prompt-bool prompt-text #:default (default (void)))
  (define default-given? (boolean? default))
  (define default-text
    (if default-given?
        (format "[~a]" (if default "Yes" "No"))
        ""))
  (define (input->option i)
    (let ((i (format "~a" i)))
      (cond [(or (equal? (string-downcase i) "y")
                 (equal? (string-downcase i) "yes")) 'true]
            [(or (equal? (string-downcase i) "n")
                 (equal? (string-downcase i) "no")) 'false]
            [else #f])))
  (display/f "~a (Yes/No)~a: " prompt-text default-text)
  (let loop ([input (read/read-line)])
    (let ([input (and (not (equal? eof input)) input)])
      (cond [(and (not input) (not default-given?))
             (display/f "~a (Yes/No)~a: " prompt-text default-text)
             (loop (read/read-line))]
            [(not input) default]
            [else (let ([as-option (input->option input)])
                    (if as-option
                        (eq? 'true as-option)
                        (begin
                          (display/f "Invalid option: ~a~n~a (Yes/No)~a: "
                                     input prompt-text default-text)
                          (loop (read/read-line)))))]))))

;; Shows a prompt for text input using (current-input-port)
;; and (current-output-port)
(define (prompt-input #:text (prompt-text #f))
  (let ((prompt-text (or prompt-text "Input")))
    (display/f "~a: " prompt-text)
    (let loop ((input (read/read-line)))
      (if input
          (format "~a" input)
          (begin (display/f "~a: " prompt-text)
                 (loop (read/read-line)))))))


;; tty check adapted from
;; http://lists.racket-lang.org/users/archive/2003-January/001719.html

;; Attempt at cross-platform compatability
(define on-windows? (eq? (system-type) 'windows))
(define tty-prog  (if on-windows? "tty.exe"  "tty"))

;; Is this program being run on the command line?
(define tty?
  (let*-values
      (((p pout pin perr)
        (subprocess
         (current-output-port)
         (current-input-port)
         (current-error-port)
         (find-executable-path tty-prog #f)
         "-s")))
    (subprocess-wait p)
    (zero? (subprocess-status p))))

;; Like prompt-input, except hides the input from view
(define (prompt-password #:text (prompt-text #f))
  ;; Capture existing stty settings to restore later
  (let ((prompt-text (or prompt-text "Password")))
    (define (display-prompt)
      (display/f "~a: " prompt-text)
      (flush-output))
    ;; Shows prompt and gets a user password
    (define (prompt-and-read)
      (display-prompt)
      (read/read-line #:reader read-password))
    (let ((password (let loop ((input (prompt-and-read)))
                      (or input
                          (begin (loop (prompt-and-read)))))))
      ;; Restore stty settings
      (format "~a" password))))

;; Handle C-c presses
(define (on-break exn)
  (let ([go? (begin
               (semaphore-wait commit-lock)
               (or committing?
                   (begin
                     (semaphore-post commit-lock)
                     (or (abort-commit-dialogue)
                         ((exn:break-continuation exn))))))])
    (when go?
      (custodian-shutdown-all comm-cust)
      (exit 0))))

;; Prompts the user if they are aborting mid-commit
(define (abort-commit-dialogue)
  (displayln/err "The commit action is in progress.")
  (displayln/err "Cancelling now may or may not work.")
  (displayln/err "Cancel anyway?")
  (let ((choice (prompt '("Continue Commit" "Try to Cancel"))))
    (not (equal? choice "Continue Commit"))))

;; Displays error messages
(define (report-error tag exn)
  (let* ([msg (if (exn? exn)
                  (let ([s (exn-message exn)])
                    (if (string? s) s (format "~.s" s)))
                  (format "~.s" exn))]
         [retry? (regexp-match #rx"bad username or password for" msg)])
    (custodian-shutdown-all comm-cust)
    (displayln/err/f "Server Error: ~a" msg)
    (if retry?
        (begin (init-comm) (semaphore-post go-sema))
        (exit 0))))

(define (submit-file username password assignment content)
  (define final-message "Handin successful.")
  (define success? #t)
  (sync (thread
         (lambda ()
           (with-handlers ([exn:fail? (lambda (e)
                                        (set! success? #f)
                                        (set! final-message (exn->string e)))])
             (submit-assignment
              connection
              username
              password
              assignment
              content
              ;; on-commit
              (lambda ()
                (semaphore-wait commit-lock)
                (display "Committing...")
                (set! committing? #t)
                (semaphore-post commit-lock))
              ;; message/message-final/message-box handlers
              (lambda (msg) (display msg))
              (lambda (msg) (set! final-message msg))
              (lambda (msg styles) (displayln msg)))))))
  (display final-message)
  (set! committing? #f)
  (exit (if success? 0 1)))

(define (retrieve-file username password assignment on-retrieve)
  (let ([buf #f])
    (sync (thread
           (lambda()
             (set! buf (retrieve-assignment
                        connection
                        username
                        password
                        assignment)))))
    (on-retrieve buf)
    (exit 0)))

;; Like retrieve-file, but saves to a file destination
(define (retrieve-file/save username password assignment
                            dest-path #:overwrite? (overwrite? #f))
  (define (on-retrieve buf)
    (let ((buf (wxme->input (open-input-bytes buf))))
      (define file (open-output-file/safe
                    dest-path #:overwrite? overwrite?))
      (write-bytes (port->bytes buf) file)
      (close-output-port file)))
  (retrieve-file username password assignment on-retrieve))

(define (overwrite-prompt filename)
  (prompt-bool (format "File `~a' already exists. Overwrite?" filename)
               #:default #f))

(define (open-output-file/safe path #:overwrite? (always-overwrite? #f))
  (define (open-with-flag exists-flag)
    (open-output-file path #:exists exists-flag))
  (define (open/overwrite) (open-with-flag 'replace))
  (define (open/error)
    (open-with-flag (if (maybe-prompt)
                        'replace
                        'error)))
  (define (maybe-prompt)
    (and (file-exists? path)
         (overwrite-prompt path)))
  (if always-overwrite?
      (open/overwrite)
      (open/error)))




;; Initializes connection to the handin server
(define (init-comm)
  (display/f "Connecting to ~a..." server)
  (flush-output)
  (set! go-sema (make-semaphore 1))
  (set! commit-lock (make-semaphore 1))
  (set! comm-cust (make-custodian))
  (set! login-sema (make-semaphore 0))
  (parameterize ([current-custodian comm-cust])
    (thread
     (lambda ()
       (let/ec break
         (with-handlers ([void
                          (lambda (exn)
                            (report-error "Connection failed." exn)
                            (break))])
           (semaphore-wait go-sema)
           (let* ([h (connect)]
                  [l (retrieve-active-assignments h)]
                  [n (cond [(member (remembered-assignment) l)
                            => (lambda (r) (- (length l) (length r)))]
                           [else #f])])
             (when (null? l)
               (handin-disconnect h)
               (error 'handin "there are no active assignments"))
             (set! connection h)
             (set! assignments (append assignments l))
             (set! default-assignment n)
             (displayln/f "connected securely for ~a." handin-name)
             (flush-output)
             (semaphore-post login-sema))))))))

;; Like assq, but returns the cdr if the result is non-false
(define (assq/cdr sym lst)
  (let ((res (assq sym lst)))
    (and res (cdr res))))

;; Preps the given file for sending to the handin server
(define (file->wxme file)
  (let* ([base (make-object editor-stream-out-bytes-base%)]
         [stream (make-object editor-stream-out% base)]
         [text (make-object text%)])
    (write-editor-version stream base)
    (write-editor-global-header stream)
    (send text insert-file file)
    (send text write-to-file stream)
    (write-editor-global-footer stream)
    (send base get-bytes)))

;; Reformats the given handin server response to a normal text file
(define wxme->input wxme-port->text-port)


(define (init-and-submit given-args)
  (init-comm)
  (semaphore-wait login-sema)
  (let ((username (or (assq/cdr 'username given-args)
                      (prompt-input #:text "Username")))
        (password (or (assq/cdr 'password given-args)
                      (prompt-password)))
        (assignment (or (assq/cdr 'assignment given-args)
                        (prompt assignments
                                #:text "Select an assignment"
                                #:default default-assignment)))
        (to-submit (or (assq/cdr 'to-submit given-args)
                       (prompt-input #:text "File to submit"))))
    (with-handlers ([exn:break? on-break])
      (submit-file username password assignment (file->wxme to-submit)))))

(define (init-and-retrieve given-args)
  (sync (init-comm))
  (semaphore-wait login-sema)
  (let ((username (or (assq/cdr 'username given-args)
                      (prompt-input #:text "Username")))
        (password (or (assq/cdr 'password given-args)
                      (prompt-password)))
        (assignment (or (assq/cdr 'assignment given-args)
                        (prompt assignments
                                #:text "Select an assignment"
                                #:default default-assignment)))
        (dest-file (or (assq/cdr 'to-submit given-args)
                       (prompt-input #:text "File to submit")))
        (overwrite? (assq/cdr 'overwrite? given-args)))
    (retrieve-file/save username password assignment dest-file #:overwrite? overwrite?)))
