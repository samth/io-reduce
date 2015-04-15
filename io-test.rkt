#lang racket

(require racket/async-channel
         racket/match
         rackunit
         mzlib/etc
         racket/port
         file/gunzip)

(define-syntax (in-this-directory stx)
  (syntax-case stx ()
    [(_ e1 e2 ...)
     (with-syntax ([cwd (syntax/loc stx
                          (this-expression-source-directory e1))])
       #'(parameterize ([current-directory cwd])
           e1 e2 ...))]))

(define (rm-rf path)
  (when (or (file-exists? path) (directory-exists? path))
    (delete-directory/files path)))

(define keep-new-directories?
  (make-parameter #f (lambda (new-b)
                       (if (not (boolean? new-b))
                           (raise-type-error 'keep-new-directories? "boolean" new-b)
                           new-b))))

(define-syntax in-new-directory
  (syntax-rules ()
    [(_ dir-e e1 e2 ...)
     (let ([dir dir-e])
       (dynamic-wind
        void
        (lambda ()
          (when (directory-exists? dir)
            (error 'in-new-directory "can't create directory ~a; directory exists" dir))
          (make-directory* dir)
          (parameterize ([current-directory dir]
                         [keep-new-directories? #t])
            e1 e2 ...))
        (lambda ()
          (unless (keep-new-directories?)
            (rm-rf dir)))))]))


;; This procedure must create a separate thread because clients may read
;; from the input port lazily, but the transform procedure eagerly reads
;; all input from the underlying port.

;; However, if an exception is raised by the transformer, that exception
;; should be propagated to the client thread. So we catch the exception,
;; send it to a shared channel, and hang onto it to be rereaised in the
;; client thread when the client reaches that point in the input.

(struct filter-error (value))

(define (make-filter-input-port/debug transform in close-orig? history)
  (let-values ([(pipe-input pipe-output) (make-pipe)])
    (let* ([chan (make-channel)]
           [handler (lambda (exn)
                      (close-output-port pipe-output)
                      (channel-put chan (filter-error exn))
                      (when history
                        (async-channel-put history `(exn transformer ,exn))))])
      (thread (lambda ()
                (with-handlers ([(lambda (exn) #t) handler])
                  (transform in pipe-output)
                  (when history
                    (async-channel-put history 'done-transform))
                  (close-output-port pipe-output))))
      (make-input-port (object-name in)
                       (lambda (buffer)
                         (let ([count (read-bytes-avail!* buffer pipe-input)])
                           (cond
                             [(and (eof-object? count) (channel-try-get chan))
                              => (lambda (err)
                                   (raise (filter-error-value err)))]
                             [else count])))
                       #f
                       (lambda ()
                         (close-input-port pipe-input)
                         (when close-orig? (close-input-port in)))))))


(struct running-script (thread channel port file-port))


;; start-script : (union path string) -> running-script
;; NOTE: *only* works with big.zip and broken.zip
(define (start-script path)
  (let* ([history (make-async-channel 100)]
         [file-in (open-input-file path)]
         [filter-in (make-filter-input-port/debug inflate file-in #f history)])
    (file-position file-in #x26)
    (running-script
     (thread
      (lambda ()
        (let ([handler (lambda (e)
                         (sleep 1)
                         (async-channel-put history `(exn main-thread ,e)))])
          (with-handlers ([exn? handler])
            (async-channel-put history 'trying-to-read)
            (let loop ([i 0])
              (when (> i 4440)
                (async-channel-put history `(reading-line ,i)))
              (cond
                [(read-line filter-in 'any)
                 => (lambda (line)
                      (unless (eof-object? line)
                        (loop (add1 i))))]))
            (async-channel-put history 'done-reading)))))
     history
     filter-in
     file-in)))

;; script-wait : running-script -> (listof event)
;; waits for a running script to finish, cleans up, and returns the transcript
(define (script-wait running)
  (thread-wait (running-script-thread running))
  (close-input-port (running-script-port running))
  (close-input-port (running-script-file-port running))
  (let loop ([result '()])
    (let ([event (async-channel-try-get (running-script-channel running))])
      (if (not event)
          (reverse result)
          (loop (cons event result))))))

;; check-script : running-script (listof event) -> ?
(define (check-script running expected)
  (let ([transcript (script-wait running)])
    (check-equal? (length transcript) (length expected)
           (format "expected: ~v, actual: ~v" expected transcript))))

(define (make-broken-copy from to k)
  (with-input-from-file from
    (lambda ()
      (let ([in (make-limited-input-port (current-input-port) k)])
        (with-output-to-file to
          (lambda ()
            (copy-port in (current-output-port))))))))

(in-this-directory
 (in-new-directory "sandbox"
                   (make-broken-copy (build-path 'up "examples" "big.zip")
                                     "broken.zip"
                                     42440)
                   (check-script
                    (start-script "broken.zip")
                    `(trying-to-read
                      (exn transformer ,exn:fail?)
                      (exn main-thread ,exn:fail?)))))

