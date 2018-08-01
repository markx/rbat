#lang racket


(define host (make-parameter "bat.org"))
(define port (make-parameter 23))


(define (handle-conn in out)
  (define (loop)
    (sync (handle-evt
           in
           (lambda (_)
             (define line (read-line-avail in))
             (unless (eof-object? line)
               (handle-line line)
               (loop))))
          (handle-evt
           (current-input-port)
           (lambda (_)
             (displayln (read-line) out )
             (flush-output out)
             (loop)))))
  (loop))


(define (handle-line line)
  (display line)
  (flush-output))

(define (read-line-avail in)
  (let loop ([buf ""])
    (if (and (> (string-length buf) 0)
             (not (char-ready? in)))
        buf
        (begin
          (let ([c (read-char in)])
            (cond
             [(eof-object? c)
              (if (zero? (bytes-length buf))
                  eof
                  buf)]
             [(eqv? c #\newline)
              (string-append buf "\n")]
             [else (loop (string-append buf (string c)))]))))))


(define (main-loop)
  (define conn-thread (thread (λ ()
                                (define-values (in out) (tcp-connect (host) (port)))
                                (handle-conn in out))))
  (thread-wait conn-thread))


(main-loop)


