#lang racket

(require file/gunzip)

;; This procedure must create a separate thread because clients may read
;; from the input port lazily, but the transform procedure eagerly reads
;; all input from the underlying port.

;; However, if an exception is raised by the transformer, that exception
;; should be propagated to the client thread. So we catch the exception,
;; send it to a shared channel, and hang onto it to be rereaised in the
;; client thread when the client reaches that point in the input.


[define file-in (open-input-file "broken.zip")]
[define-values (pipe-input pipe-output) (make-pipe)]
(thread (lambda ()
          (with-handlers ([exn? (lambda (exn) 
                                  (printf "pos: ~a\n" (file-position file-in))
                                  (close-output-port pipe-output))])
            (inflate file-in pipe-output)
            (close-input-port pipe-input)
            (close-output-port pipe-output))))
[define filter-in 
  (make-input-port #f
                   (lambda (buffer)
                     (define r (read-bytes-avail!* buffer pipe-input))
                     ;(printf "read: ~a ~a ~a\n" r (file-position file-in) (file-position filter-in))
                     r)
                   #f
                   (lambda () (close-input-port pipe-input)
                           (close-output-port pipe-output)))]

(file-position file-in #x26)

(thread-wait   
 (thread
  (lambda ()
    (let loop ()
      (define d (read-line filter-in 'any))
      (unless (eof-object? d) (loop))))))

