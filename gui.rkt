#lang racket/gui

(provide
 gui%)
  

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


(define gui% 
  (class object%
    (init on-send)
    (define frame null)
    (define window-general null)
    (define window-chat null)
    (define prompt null)
    (super-new)

    (define/public (run)
      (set! frame (new frame%
                       [label "rbat"]
                       [width 900]
                       [height 600]))

      (define message-window (new horizontal-pane% [parent frame]))
      (set! window-general (make-window-general message-window))
      (define right-pane (new vertical-panel% 
                              [parent message-window]))
      (set! window-chat (make-window-chat right-pane))

      (set! prompt (make-prompt frame))
      (send frame show #t))

    (define/public (send-to-window window content)
      (case window
        [("chat") (send (send window-chat get-editor) insert content)]
        [else (send (send window-general get-editor) insert content)]))))
    
