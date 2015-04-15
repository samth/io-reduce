#lang racket

(require racket/async-channel file/gunzip)

;; This procedure must create a separate thread because clients may read
;; from the input port lazily, but the transform procedure eagerly reads
;; all input from the underlying port.

;; However, if an exception is raised by the transformer, that exception
;; should be propagated to the client thread. So we catch the exception,
;; send it to a shared channel, and hang onto it to be rereaised in the
;; client thread when the client reaches that point in the input.

(struct filter-error (value))

(define (make-filter-input-port/debug transform in)
  (let-values ([(pipe-input pipe-output) (make-pipe)])
    (let* ([chan (make-channel)])
      (thread (lambda ()
                (with-handlers ([exn? (lambda (exn) (close-output-port pipe-output))])
                  (transform in pipe-output))))
      (make-input-port (object-name in)
                       (lambda (buffer) (read-bytes-avail!* buffer pipe-input))
                       #f
                       (lambda () (close-input-port pipe-input))))))


;; start-script : (union path string) -> running-script
;; NOTE: *only* works with big.zip and broken.zip
(define (start-script path)
  (let* ([history (make-async-channel 100)]
         [file-in (open-input-file path)]
         [filter-in (make-filter-input-port/debug inflate file-in)])
    (file-position file-in #x26)
    (thread
     (lambda ()
       (let loop ()
         (cond
           [(read-line filter-in 'any)
            => (lambda (line)
                 (unless (eof-object? line)
                   (loop)))]))))))

(thread-wait (start-script "broken.zip"))

