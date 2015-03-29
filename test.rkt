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
    (send this set-max-undo-history 0)
    (file-position _file eof)
    (define startpos (file-position file))
    (define endpos (file-position file))
    (define buffersize (expt 2 8))
    (define movemargin (/ buffersize 4))
    (define (trim-buffer)
      ;; implement
      #f
      )
    (define (update)
      (let ([start (box 0)]
            [end (box 0)])
        (get-visible-position-range start end)
        (when (< (unbox start) movemargin)
          ;; too close to the top, try to prepend more data
          (trim-buffer)
          (let* ([prependsize (- buffersize (- endpos startpos))]
                 [prependstart (- startpos prependsize)])
            (file-position _file (max 0 prependstart))
            ;; step forward until next utf8 start
            ;; step at most 6 bytes
            (let/cc break
                    (for ([i (in-range 6)])
                      (let ([byte (peek-byte _file)])
                        (cond
                         [(< byte 128) (break)]
                         [else (read-byte _file)]))))
            (let* ([newstart (file-position _file)]
                   [newsize (- prependsize (- newstart prependstart))]
                   ;; read bytes until we find a utf8 start
                   [data (let rec ([data (read-bytes newsize _file)]
                                   [limit 6])
                           (cond
                            [(< limit 1) data]
                            [(eof-object? (peek-byte _file)) data]
                            [(< (peek-byte _file) 128) data]
                            [else (rec (bytes-append data (make-bytes (read-byte _file))) (- limit 1))]))])
              (begin-edit-sequence #f #f)
              (insert (bytes->string/utf-8 data #\?) 0 'same #f)
              (end-edit-sequence)
              (set! startpos newstart))))
        (when (> (unbox end) (- buffersize movemargin))
          ;; too close to the bottom, try to append more data
          (trim-buffer)
          (let ([appendsize (- buffersize (- endpos startpos))])
            #f
            )
          )
        ))
    (define (on-change)
      ;;(queue-callback update)
      ;; delay the update a little bit
      (new timer% [notify-callback update]
           [interval 100]
           [just-once? #t]))
    (augment on-change)))

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

