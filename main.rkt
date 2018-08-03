#lang racket

(require "telnet.rkt")
(require "logger.rkt")


(define host (make-parameter "bat.org"))
(define port (make-parameter 23))


(define (handle-line line)
  (display line)
  (log-info (bytes->string/utf-8 line))
  (flush-output))


(define (read-line-avail in)
  (let loop ([buf (bytes)])
    (if (and (> (bytes-length buf) 0)
             (not (byte-ready? in)))
        buf
        (begin
          (let ([b (read-byte in)])
            (cond
             [(eof-object? b)
              (if (zero? (bytes-length buf))
                  eof
                  buf)]

             [(= b (char->integer #\newline))
              (bytes-append buf #"\n")]

             [else
              (loop (bytes-append buf (bytes b)))]))))))


(define (main-loop)
  (define-values (in out) (telnet-connect (host) (port)))
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
             (displayln (read-line) out)
             (flush-output out)
             (loop)))))

          
  (loop))

(module+ main
  (main-loop))

