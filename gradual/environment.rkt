#lang racket

;; This module create the base enviroments for the user program

(provide (all-defined-out))
(require
 (for-syntax
  syntax/parse
  ;;racket/base
  "./types.rkt"))

(define-syntax (gradual-datum stx)
  ;;(print 'datum) (newline)
  (syntax-case stx ()
    [(_ . s)
     (let ([d (syntax->datum #'s)])
       ;; I only wish to deal with a few primitives for now
       (if (or (fixnum? d) (boolean? d))
           (syntax/loc stx (#%datum . s))
           (raise-syntax-error #'s)))]))

(define-syntax (gradual-lambda stx)
  ;;(print 'lambda) (newline)
  (syntax-parse stx
    [(lam (fml:lambda-formal ...) body)
     (begin (display (attribute fml.name))
            (quasisyntax/loc stx (lambda #,(attribute fml.name) body)))]))

(define-syntax (gradual-apply stx)
  ;;(print 'apply) (newline)
  (syntax-parse stx
    [(app rator rands ...) (syntax/loc stx (#%plain-app rator rands ...))]))

;; mimic the first order nature of schml primitives
(require racket/fixnum)
(define-syntax (gradual-plus stx)
  ;;(print 'plus) (newline)
  (syntax-parse stx
    [(p n m) #'(fx+ n m)]))

(define-syntax (gradual-minus stx)
 ;; (print 'minus) (newline)
  (syntax-parse stx
    [(p n m) #'(fx- n m)]))
