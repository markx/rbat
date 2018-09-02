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

(define gui% 
  (class object%
    (define frame null)
    (define window-general null)
    (define window-chat null)
    (define prompt null)
    (define on-send null)
    (super-new)

    (define (on-textbox-changed t e)
      (and (eq? (send e get-event-type) 'text-field-enter)
           (let ([msg (send t get-value)])
               (on-send msg)

             (send (send window-general get-editor) insert msg)
             (send t set-value ""))))

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

      (set! prompt  (new text-field% 
                         [parent frame] 
                         [label ">"]
                         [callback on-textbox-changed]))
      (send frame show #t))

    (define/public (send-to-window window content)
      (case window
        [("chat") (send (send window-chat get-editor) insert content)]
        [else (send (send window-general get-editor) insert content)]))
    
    (define/public (set-on-send callback)
      (set! on-send callback))))
