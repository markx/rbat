#lang racket/gui

(provide
 send-to-window)
  

(define (make-window-chat parent)
  (new editor-canvas%
       [parent parent]
       [enabled #f]
       [label "chat"]
       [editor (new text%)]))

(define (make-window-general parent)
  (define text (new text%))
  (define editor-canvas (new editor-canvas%
                          [parent parent]
                          [enabled #f]
                          [style '(no-border)]
                          [min-width 600]))
  (send editor-canvas set-editor text)
  editor-canvas)

(define (make-prompt parent)
  (new text-field% [parent parent] [label ">"]))


(define (send-to-window window content)
  (case window
    [("chat") (send (send window-chat get-editor) insert content)]
    [else (send (send window-general get-editor) insert content)]))



(define frame (new frame%
                   [label "rbat"]
                   [width 900]
                   [height 600]))

(define message-window (new horizontal-pane% [parent frame]))
(define window-general (make-window-general message-window))
(define right-pane (new vertical-panel% 
                        [parent message-window]))
(define window-chat (make-window-chat right-pane))


(define prompt (make-prompt frame))

(send frame show #t)
