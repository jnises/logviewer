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
      ;; implement
      #f
      )
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
               [(< (peek-byte _file) 128) #f]
               [else (read-byte _file)
                     (loop (- limit 1))]))
            (let* ([newstart (file-position _file)]
                   [newsize (- refill-size-bytes (- newstart prependstart))]
                   [data (read-bytes newsize _file)])
              (insert (bytes->string/utf-8 data #\?) 0 'same #f)
              (set! start-pos-bytes newstart)
              (when first-update
                (scroll-to-position (get-end-position))
                (set! first-update #f)))))
        (when (> end-char (- buffer-size-char move-margin-char))
          ;; too close to the bottom, try to append more data
          (trim-buffer-start)
          (let ([appendsize (- buffer-size-char (- end-pos-bytes start-pos-bytes))])
            ;; read bytes until we find a utf8 start
                           ;;     [data (let rec ([data (read-bytes newsize _file)]
                           ;;         [limit 6])
                           ;; (cond
                           ;;  [(< limit 1) data]
                           ;;  [(eof-object? (peek-byte _file)) data]
                           ;;  [(< (peek-byte _file) 128) data]
                           ;;  [else (rec (bytes-append data (make-bytes (read-byte _file))) (- limit 1))]))]
            #f
            )))
      (end-edit-sequence))
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

