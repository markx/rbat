#lang racket

(define host (make-parameter "bat.org"))
(define port (make-parameter 23))


(define cmd-SE    240)
(define cmd-NOP   241)
(define cmd-DM    242)
(define cmd-BRK   243)
(define cmd-IP    244)
(define cmd-AO    245)
(define cmd-AYT   246)
(define cmd-EC    247)
(define cmd-EL    248)
(define cmd-GA    249)
(define cmd-SB    250)
(define cmd-WILL  251)
(define cmd-WONT  252)
(define cmd-DO    253)
(define cmd-DONT  254)
(define cmd-IAC   255)


(define (read-cmd in)
  (define b (read-byte in))
  (cond
   [(eq? b cmd-GA) #f]
   [(or (eq? b cmd-DO)
        (eq? b cmd-DONT)
        (eq? b cmd-WILL)
        (eq? b cmd-WONT))
    (read-byte in)]
   ))


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


