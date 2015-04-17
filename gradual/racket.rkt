#lang racket/base

;; This module imports the base enviroments for user programs
;; and calls the type checker and cast inserter function before
;; expansion/compilation of the program has concluded.

;; 1) Provide the initial environment.
(require syntax/parse)
(require "./types.rkt")
(require "./environment.rkt")

(provide
 #%plain-module-begin
 (rename-out
  [gradual-minus  -]
  [gradual-plus   +]
  [gradual-datum  #%datum]
  [gradual-apply  #%app]
  [gradual-lambda lambda]))

;; 2) Rebind module-begin so that we can type-check before expansion
;; concludes.

(require (for-syntax "./types.rkt"))
(require (for-syntax racket/base))

(define-syntax (gradual-module-begin stx)
  (define (expand stx)
    (let ([stx (quasisyntax/loc stx (#%plain-module-begin #,stx))])
      (local-expand stx 'module-begin `(,#'module*))))
  ;;(print-syntax-width +inf.0)
  (print 'module-begin) (newline)
  (syntax-case stx ()
    [(_) #'(#%plain-module-begin)]
    [(_ exp)
     (with-syntax ([(_  core-exp) (expand #'exp)])
       (let*-values ([(cast-exp type) (type-check/insert-casts #'core-exp)])
       ;; I think that I could switch this to a #%plain-module-begin
       ;; and add my own printer.
       #`(#%module-begin
           #,cast-exp
          ;; this is just a hack for now
          #,(format ": ~a" type))))]
    [(_ exp ...)
     (raise-syntax-error
      'gradual/racket
      "Currently only handles single expressions.")]))

(provide (rename-out [gradual-module-begin #%module-begin]))
