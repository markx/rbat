#lang racket/base

(require racket/string)
(require racket/format)
(require gregor)

(define log-receiver (make-log-receiver (current-logger) 'debug))


(define (make-timestamp)
  (datetime->iso8601  (now)))

(void 
 (thread 
  (λ()
    (call-with-output-file "messages.log"
      (λ (out)
        (let loop () 
          (define v (sync log-receiver))
          (fprintf out "[~a]~a: ~a\n"
                   (~a #:width 4 (vector-ref v 0))
                   (~a #:width 23 (make-timestamp))
                   (string-trim (vector-ref v 1) #rx"\r?\n")) 

          (flush-output out)
          (loop)))
      #:exists 'append))))

