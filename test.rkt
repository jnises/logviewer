#lang racket/gui

;; TODO handle the fact that editor positions are in characters, and the file position is in bytes
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
    (inherit get-end-position)
    (inherit delete)
    (send this set-max-undo-history 0)
    (file-position _file eof)
    (define start-pos-bytes (file-position file))
    (define end-pos-bytes (file-position file))
    (define buffer-size-char (expt 2 10))
    (define move-margin-char (/ buffer-size-char 4))
    (define refill-size-bytes (* 2 move-margin-char))
    (define trim-margin-char (/ buffer-size-char 8))
    (define first-update #t)
    (define update-timer #f)
    (define (get-visible-range)
      (let ([start (box 0)]
            [end (box 0)])
        (get-visible-position-range start end)
        (list (unbox start) (unbox end))))
    (define (trim-buffer-start)
      (match-let ([(list visible-start _) (get-visible-range)])
        (let ([trim-end (max 0 (- visible-start trim-margin-char))])
          (delete 0 trim-end #f))))
    (define (trim-buffer-end)
      (match-let ([(list _ visible-end) (get-visible-range)])
        (let ([trim-start (- (get-end-position) (max 0 (- (get-end-position) visible-end trim-margin-char)))])
          (delete trim-start (get-end-position) #f))))
    (define (update)
      (begin-edit-sequence #f #f)
      (match-let ([(list start-char end-char) (get-visible-range)])
        (when (< start-char move-margin-char)
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
                   [data (read-bytes readsize _file)])
              (insert (bytes->string/utf-8 data #\?) 0 'same #f)
              (set! start-pos-bytes newstart))))
        (when (> end-char (- (get-end-position) move-margin-char))
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
            (set! end-pos-bytes (file-position _file))
            (when (not (eof-object? data))
              (insert (bytes->string/utf-8 data #\?) 0 'same #f)))))
      (end-edit-sequence)
      (when first-update
        (scroll-to-position (get-end-position))
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

