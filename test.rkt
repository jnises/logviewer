#lang racket/gui

(define logtext%
  (class text%
    (init file)
    (super-new [auto-wrap #t])
    (define _file file)
    (inherit get-visible-position-range)
    (inherit insert)
    (inherit begin-edit-sequence)
    (inherit end-edit-sequence)
    (inherit scroll-to-position)
    (inherit last-position)
    (inherit delete)
    (inherit get-text)
    (send this set-max-undo-history 0)
    (file-position _file eof)
    (define start-pos-bytes (file-position file))
    (define end-pos-bytes (file-position file))
    (define buffer-size-char (expt 2 16))
    (define move-margin-char (/ buffer-size-char 32))
    (define refill-size-bytes (/ buffer-size-char 16))
    (define trim-margin-char (/ buffer-size-char 16))
    (define first-update #t)
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
        (let ([trim-start (- (last-position) (max 0 (- (last-position) visible-end trim-margin-char)))])
          ;; TODO keep track of the bytes in some other way?
          ;; non-utf-8 chars are converted to ? and should thus be counted correctly
          (set! end-pos-bytes (- end-pos-bytes (bytes-length (string->bytes/utf-8 (get-text trim-start (last-position))))))
          (delete trim-start (last-position) #f))))
    (define (update)
      (begin-edit-sequence #f #f)
      (match-let ([(list start-char end-char) (get-visible-range)])
        (when (and (< start-char move-margin-char))
          ;; too close to the top, try to prepend more data
          (trim-buffer-end)
          (let* ([prependstart (- start-pos-bytes refill-size-bytes)])
            (file-position _file (max 0 prependstart))
            ;; step forward until next utf8 start
            ;; step at most 6 bytes
            (let loop ([limit 6])
              (cond
               [(< limit 1) #f]
               [(eof-object? (peek-byte _file)) #f]
               [(< (peek-byte _file) 128) #f]
               [else (read-byte _file)
                     (loop (- limit 1))]))
            (let* ([newstart (file-position _file)]
                   [readsize (- start-pos-bytes newstart)]
                   [data (read-bytes readsize _file)]
                   [text (bytes->string/utf-8 data #\?)])
              (insert text 0 'same #f)
              (scroll-to-position (+ start-char (string-length text)))
              (set! start-pos-bytes newstart))))
        (when (> end-char (- (last-position) move-margin-char))
          ;; too close to the bottom, try to append more data
          (trim-buffer-start)
          (file-position _file end-pos-bytes)
          ;; make sure we end at a utf-8 boundary
          (let ([data (let loop ([limit 6]
                                 [data (read-bytes refill-size-bytes _file)])
                        (cond
                         [(< limit 1) data]
                         [(eof-object? (peek-byte _file)) data]
                         [(< (peek-byte _file) 128) data]
                         [else (loop (- limit 1) (bytes-append data (make-bytes (read-byte _file))))]))])
            (when (not (eof-object? data))
              (set! end-pos-bytes (file-position _file))
              (insert (bytes->string/utf-8 data #\?) (last-position) 'same #f)))))
      (end-edit-sequence)
      (when first-update
        (scroll-to-position (last-position))
        (set! first-update #f)))
    ;; TODO do updates in some smarter way than this
    (set! update-timer (new timer% [notify-callback update]
                            [interval 100]))))

(define frame (new frame% 
                   [label "Example"]
                   [width 400]
                   [height 400]))
 
(define c (new editor-canvas% [parent frame]))
(define t (new logtext% [file (open-input-file "/tmp/testfile")]))
(send c set-editor t)

(define mb (new menu-bar% [parent frame]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(append-editor-operation-menu-items m-edit #f)

(send frame show #t)

