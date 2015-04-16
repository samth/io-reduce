#lang racket

(require file/gunzip)
(define file-in (open-input-file "broken.zip"))
(file-position file-in #x26)
 
(inflate file-in (open-output-nowhere))
