#lang s-exp syntax/module-reader
gradual/racket
#|
#:language-info make-language-info
#:info make-info

(define (make-info key default use-default)
  (case key
    [else (use-default key default)]))

(define make-language-info
  `#(gradual-racket/language-info get-info ()))
|#
