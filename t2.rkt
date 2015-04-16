#lang racket
(require file/gunzip)
[define file-in (open-input-file "broken.zip")]
[define-values (pipe-input pipe-output) (make-pipe)]


(thread (lambda ()
          (with-handlers ([exn? (lambda (exn) 
                                  (printf "pos: ~a\n" (file-position file-in))
                                  (close-output-port pipe-output))])
            (inflate file-in pipe-output)
            (close-input-port pipe-input)
            (close-output-port pipe-output))))
(file-position file-in #x26)

(thread-wait   
 (thread
  (lambda ()
    (let loop ()
      (define d (read-line pipe-input 'any))
      (unless (eof-object? d) (loop))))))


