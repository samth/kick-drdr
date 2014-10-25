#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/page
         json)

(define tokens (file->lines "token"))

(define cmd "racket -t main")

(define (main-process)
  (define lines (regexp-split "\n" (with-output-to-string (lambda () (system "ps axf")))))
  (define procs (filter (lambda (l) (regexp-match cmd l)) lines))
  (define pids (map (lambda (l) (define d (regexp-match #px"\\w*\\d+" l)) (and (pair? d) (string->number (car d)))) procs))
  pids)

(define (string->number* s)
  (or (string->number s)
      (error 'children "expected a numeric string, got ~a" s)))

(define (not-empty s)
  (not (equal? s "")))

(define (children pid)
  (unless (number? pid)
    (error 'children "expected a number, but got ~a" pid))
  (define immediate 
    (map string->number* (filter not-empty (regexp-split "\n" (with-output-to-string (lambda () (system (format "pgrep -P ~a" pid))))))))
  (define descendants (append-map children immediate))
  (append immediate descendants))

(define (kill pids)
  (display ">>> ")
  (define cmd (format "kill -9 ~a" (apply ~a (for/list ([p pids]) (format " ~a" p)))))
  (displayln cmd)
  (system cmd))


(define (start req)
  (define p-token (get-binding 'token req))
  (unless (member p-token tokens)
    (error 'restart-drdr "bad token: ~a ~a" tokens p-token))
  (printf ">>> restarting drdr\n")
  (define pids (main-process))
  (define kids (append-map children pids))
  (unless (andmap number? (append pids kids))
    (error 'restart-drdr "bad children: ~a" kids))
  (kill (append pids kids))
  (response/full 200 #"Okay" (current-seconds)
                 #"application/json" null
                 (list (string->bytes/utf-8 (format "DrDr has been kicked, pids were ~a." pids)))))

(serve/servlet 
 start
 #:port 8080 #:listen-ip #f
 #:servlet-path "/restart-drdr" #:command-line? #t)
