#lang racket

(require "telnet.rkt")
(require "logger.rkt")
(require "gui.rkt")

(define host (make-parameter "bat.org"))
(define port (make-parameter 23))

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


(define (main)
  (define-values (in out) (telnet-connect (host) (port)))
  (define gui (new gui%)) 
  (send gui set-on-send (lambda (cmd) 
                          (displayln cmd out)
                          (flush-output out)))
  (send gui run)

  (define (handle-line raw-line) 
    (define line (string-replace raw-line "\r\n" "\n"))
    (log-info line)
    (send gui send-to-window "general" line))

  (define (loop)
    (sync (handle-evt
           in
           (lambda (_)
             (define line (bytes->string/utf-8 (read-line-avail in)))
             (unless (eof-object? line)
               (handle-line line)
               (loop))))

          (handle-evt
           (current-input-port)
           (lambda (_)
             (displayln (read-line) out)
             (flush-output out)
             (loop)))))

  (thread
    (lambda () (loop))))

(module+ main
  (main))
