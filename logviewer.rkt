#lang racket/gui

(define logtext%
  (class text%
    (init-field file follow)
    (super-new [auto-wrap #t])
    (send this hide-caret #t)
    (send this set-max-undo-history 0)
    (inherit get-visible-position-range)
    (inherit insert)
    (inherit begin-edit-sequence)
    (inherit end-edit-sequence)
    (inherit scroll-to-position)
    (inherit last-position)
    (inherit delete)
    (inherit get-text)
    (file-position file eof)
    (define file-size (file-position file))
    (define start-pos-bytes file-size)
    (define end-pos-bytes file-size)
    (define buffer-size-char (expt 2 16))
    (define move-margin-char (/ buffer-size-char 32))
    (define refill-size-bytes (/ buffer-size-char 128))
    (define trim-margin-char (/ buffer-size-char 16))
    (define update-timer #f)
    (define (get-visible-range)
      (let ([start (box 0)]
            [end (box 0)])
        (get-visible-position-range start end)
        (list (unbox start) (unbox end))))
    (define (trim-buffer-start)
      (match-let ([(list visible-start _) (get-visible-range)])
        (let* ([trim-end (max 0 (- visible-start trim-margin-char))]
               [text (get-text 0 trim-end)]
               [new-start (- visible-start (string-length text))])
          (set! start-pos-bytes (+ start-pos-bytes (bytes-length (string->bytes/utf-8 text))))
          (delete 0 trim-end #f)
          (scroll-to-position new-start))))
    (define (trim-buffer-end)
      (match-let ([(list _ visible-end) (get-visible-range)])
        (let ([trim-start (min (last-position) (+ visible-end trim-margin-char))])
          ;; don't need to skip any utf-8 stuff here since we are operating on chars in the editor
          ;; TODO keep track of the bytes in some other way?
          ;; non-utf-8 chars are converted to ? and should thus be counted correctly
          (set! end-pos-bytes (- end-pos-bytes (bytes-length (string->bytes/utf-8 (get-text trim-start (last-position))))))
          (delete trim-start (last-position) #f))))
    (define (update)
      (begin-edit-sequence #t #t)
      (match-let ([(list start-char end-char) (get-visible-range)]
                  [new-file-size (begin
                                   (file-position file eof)
                                   (file-position file))])
        (when (or (> end-char (- (last-position) move-margin-char)) (> new-file-size file-size))
          ;; too close to the bottom, try to append more data
          (trim-buffer-start)
          (let fillloop ()
            (file-position file end-pos-bytes)
            ;; make sure we end at a utf-8 boundary
            (let ([data (let loop ([limit 6]
                                   [data (read-bytes refill-size-bytes file)])
                          (cond
                           [(< limit 1) data]
                           [(eof-object? (peek-byte file)) data]
                           [(< (peek-byte file) 128) data]
                           [else (loop (- limit 1) (bytes-append data (make-bytes (read-byte file))))]))])
              (when (not (eof-object? data))
                (set! end-pos-bytes (file-position file))
                (insert (bytes->string/utf-8 data #\?) (last-position) 'same #f)
                (when (and (< (bytes-length data) 0) (< (last-position) buffer-size-char))
                  (fillloop))))))
        (when (< start-char move-margin-char) 
          ;; too close to the top, try to prepend more data
          (trim-buffer-end)
          (let ([prepended
                 (let fillloop ([prepended 0])
                   (let ([prependstart (- start-pos-bytes refill-size-bytes)])
                     (file-position file (max 0 prependstart))
                     ;; step forward until next utf8 start
                     ;; step at most 6 bytes
                     (let loop ([limit 6])
                       (cond
                        [(< limit 1) #f]
                        [(eof-object? (peek-byte file)) #f]
                        [(< (peek-byte file) 128) #f]
                        [else (read-byte file)
                              (loop (- limit 1))]))
                     (let* ([newstart (file-position file)]
                            [readsize (- start-pos-bytes newstart)]
                            [data (read-bytes readsize file)]
                            [text (bytes->string/utf-8 data #\?)])
                       (insert text 0 'same #f)
                       (set! start-pos-bytes newstart)
                       (let ([total-prepended (+ (string-length text) prepended)])
                         ;; loop until the buffer is big enough
                         (if (and (> (bytes-length data) 0) (< (last-position) buffer-size-char))
                             (fillloop total-prepended)
                             total-prepended)))))])
            (scroll-to-position (+ start-char prepended))))
        (set! file-size new-file-size))
      (end-edit-sequence)
      ;; TODO only do this when the file has grown?
      (when follow
        (scroll-to-position (last-position))))
    ;; read only
    (define/override (on-char event)
      #f)
    (define/public (tail value)
      (set! follow value))
    ;; TODO do updates in some smarter way than this
    (set! update-timer (new timer% [notify-callback update]
                            [interval 300]))))

(define frame (new frame% 
                   [label "Log viewer"]
                   [width 400]
                   [height 400]))
(define panel (new vertical-panel%
                   [parent frame]
                   [alignment '(left center)]))
(define logger #f)
(define follow (new check-box%
                    [parent panel]
                    [label "Tail"]
                    [value #t]
                    [callback (λ (checkbox event)
                                (when logger
                                  (send logger tail (send checkbox get-value))))]))
(define c (new editor-canvas% [parent panel]))
(define (open-log . _)
  (let ([file (get-file)])
    (when file
      (set! logger (new logtext%
                        [file (open-input-file file)]
                        [follow (send follow get-value)]))
      (send c set-editor logger))))

(define menu-bar (new menu-bar% [parent frame]))
(define file-menu (new menu%
                       [label "File"]
                       [parent menu-bar]))
(define open-file-menu (new menu-item% 
                            [label "Open Log"]
                            [parent file-menu]
                            [shortcut #\o]
                            [callback open-log]))
;; (define edit-menu (new menu%
;;                        [label "Edit"]
;;                        [parent menu-bar]))
;; (append-editor-operation-menu-items edit-menu #f)

(send frame show #t)

