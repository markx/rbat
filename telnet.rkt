#lang racket

(provide
  cmd-SE
  cmd-NOP
  cmd-DM
  cmd-BRK
  cmd-IP
  cmd-AO
  cmd-AYT
  cmd-EC
  cmd-EL
  cmd-GA
  cmd-SB
  cmd-WILL
  cmd-WONT
  cmd-DO
  cmd-DONT
  cmd-IAC

  telnet-connect
  make-telnet-port)
  
 
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

(define (make-telnet-port in)
  (define-values (pin pout) (make-pipe))
  (thread (λ ()
            (for ([b (in-port read-byte in)])
              (if (equal? b cmd-IAC)
                  (handle-cmd in pout)
                  (write-byte b pout)))
            (close-output-port pout)))
  pin)


;; ignore cmds
(define (handle-cmd in out)
  (define b (read-byte in))
  (cond
   [(eq? b cmd-IAC)
    (write-byte b out)]

   [(or (eq? b cmd-DO)
        (eq? b cmd-DONT)
        (eq? b cmd-WILL)
        (eq? b cmd-WONT))
    (read-byte in)]


   [(eq? b cmd-SB)
    (for ([b (in-port read-byte in)])
      #:break (and
               (eq? b cmd-IAC)
               (eq? (peek-byte in) cmd-SE))
      #f)
    (read-byte in)] ;; pop cmd-SE
    

   [else #f]))
   
 

(define (telnet-connect host port)
  (define-values (in out) (tcp-connect host port))
  (values (make-telnet-port in) out))
