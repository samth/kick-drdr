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
  (define pids (map (lambda (l) (define d (regexp-match #px"\\w*\\d+" l)) (and d (string->number d))) procs))
  pids)

(define (kill pid)
  (display ">>>"
  (displayln  (format "kill -9 ~a" pid))))


(define (start req)
  (define p-token (get-binding 'token req))
  (unless (member tokens p-token)
    (error 'restart-drdr "bad token: ~a ~a" tokens p-token))
  (printf ">>> restarting drdr\n")
  (define pids (main-process))
  (for-each kill pids)
  (response/full 200 #"Okay" (current-seconds)
                 #"application/json" null
                 (list (string->bytes/utf-8 (format "DrDr has been kicked, pids were ~a." pids)))))

(serve/servlet 
 start
 #:port 8080 #:listen-ip #f
 #:servlet-path "/restart-drdr" #:command-line? #t)
