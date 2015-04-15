#lang racket/base
(require racket/file (for-syntax racket/base)
	mzlib/etc)


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

(provide keep-new-directories?
         in-new-directory
         in-this-directory)
