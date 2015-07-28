#lang info

(define collection "librackto")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/librackto.scrbl" ())))
(define pkg-desc "Librato API client for racket")
(define version "0.1")
(define pkg-authors '(apg))
