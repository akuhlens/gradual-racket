#lang racket/base

(require syntax/parse)

(provide (all-defined-out))


;; The syntax representation of types
(struct Type ())
(struct DynT  Type ())
(struct BoolT Type ())
(struct IntT  Type ())
(struct FunT  Type (arity arguments return))

;; Because I don't want to write so many parens
(define DYN (DynT))
(define BOOL (BoolT))
(define INT (IntT))

(define-syntax-class type
  #:commit
  (pattern (~literal Dyn)  #:attr value DYN)
  (pattern (~literal Int)  #:attr value INT)
  (pattern (~literal Bool) #:attr value BOOL)
  (pattern (x:type ... (~literal ->) r:type)
           #:attr value
           (let ([xvals (syntax->list #'(x.value ...))])
             (FunT (length xvals) xvals (attribute r.value)))))

(define-splicing-syntax-class lambda-formal
  #:attributes (name)
  #:description
  "formal parameter of a lambda with or without an annotation"
  (pattern (id:id (~datum :) t:type)
           #:attr name (begin (printf "lf: ~a ~a\n" #'id #'t)
                              (syntax-property #'id 'type (attribute t.value))))
  (pattern id:id
           #:attr name (begin (printf "lf: ~a\n" #'id)
                              (syntax-property #'id 'type DYN))))


(require racket/match)
;; consistent will always be type, type -> Bool
(define (consistent? t g)
  (match* (t g)
    [((DynT) _) #t]
    [(_ (DynT)) #t]
    [((IntT) (IntT)) #t]
    [((BoolT) (BoolT)) #t]
    [((FunT t-arr t-args t-ret) (FunT g-arr g-args g-ret))
     (and (equal? t-arr g-arr)
          (andmap consistent? t-arr g-arr)
          (consistent? t-ret g-ret))]))

;; meet will always be type, type -> typ
(define (meet t g)
  (match* (t g)
    [((DynT) _) DYN]
    [(_ (DynT)) DYN]
    [((IntT) (IntT)) INT]
    [((BoolT) (BoolT)) BOOL]
    [((FunT t-arr t-args t-ret) (FunT g-arr g-args g-ret))
     ;; I should double check this there seems to be an argument
     ;; for auto currying or casting to dyn
     (if (equal? t-arr g-arr)
         (FunT t-arr (andmap meet t-arr g-arr) (meet t-ret g-ret))
         (error 'meet "inconsistent function types ~a and ~a" t g))]
    [(other wise) DYN]))

;; join will always be type, type -> type
(define (join t g)
  (match* (t g)
    [((DynT) _) g]
    [(_ (DynT)) t]
    [((IntT) (IntT)) INT]
    [((BoolT) (BoolT)) BOOL]
    [((FunT t-arr t-args t-ret) (FunT g-arr g-args g-ret))
     ;; I should double check this there seems to be an argument
     ;; for auto currying or casting to dyn
     (if (equal? t-arr g-arr)
         (FunT t-arr (andmap join t-arr g-arr) (join t-ret g-ret))
         (error 'join "inconsistent function types ~a and ~a" t g))]
    [(other wise) (error 'join "inconsistent types ~a and ~a" t g)]))

;; utilities that may need to be shared if this goes any further
(define (const a) (lambda b a))

(define stx-type
  (case-lambda
    [(stx ty) (syntax-property stx 'type ty)]
    [(stx)
     (let ([type (syntax-property stx 'type)])
       (if (Type? type)
           type
           (error 'stx-type "lookup for ~a retrieved ~a" stx type)))]))

;; simple-datum->Type takes any datum and returns the type of that datum or false
(define (simple-datum->type d)
  (cond
    [(fixnum? d) INT]
    [(boolean? d) BOOL]
    [else #f]))

;; type check and insert implicit casts
(define (type-check/insert-casts stx)
  (tc/cst stx (initial-type-environment)))

(define (tc/cst stx env)
  (print 'tc/cst) (newline)
  (parameterize ([current-context stx])
    (syntax-case stx (#%plain-app #%plain-lambda)
      [var (identifier? #'var) (values #'var (lookup env #'var))]
      [(#%plain-lambda (id ...) body) (tc/cst-lambda #'(id ...) #'body env)]
      [(#%plain-app rator rand* ...) (tc/cst-app #'rator #'(rand* ...))]
      [(kwote stx) 
       ;; Begin diagnosis of the symptoms 
       (begin (printf "-1: ~a 0: ~a 1: ~a\nf: ~a\nb: ~a\ns: ~a\n"
                      (identifier-binding #'kwote -1)
                      (identifier-binding #'kwote 0)
                      (identifier-binding #'kwote 1)
                      (free-identifier=? #'quote #'kwote)
                      (bound-identifier=? #'quote #'kwote)
                      (eq? (syntax->datum #'quote)
                           (syntax->datum #'kwote)))
       ;; Unfortunatly I have no idea what the problem is.
              (values #'(kwote stx) DYN))]
      [other (error 'tc/cst "unexpected value: ~a" #'other)])))

;; helpers to make tc/cst slightly more flexible
(define (tc/cst* stx* env)
  (for/lists (stx* ty*) ([stx (in-list stx*)])
    (tc/cst* stx env)))

;; How to typecheck a lambda
(define (tc/cst-lambda ids body env)
  ;; get type annotations on formals
  (let*-values ([(id*) (syntax->list ids)]
                [(env) (extend env id*)]
                ;; Typecheck the body with the extended environment
                [(body retT) (tc/cst body env)]
                ;; build funtion type and resulting syntax
                [(lamT) (FunT (length id*) (map stx-type id*) retT)]
                [(lam) #`(#%plain-lambda #,ids #,body)]
                [(lam) (stx-type lam lamT)])
    (values lam lamT)))

;; How to typececk a application
(define (tc/cst-app rator rand* env)
  ;; The actual well typed rule for application
  ;; Takes the type of the typecheck subparts and
  ;; returns the type they should be casted to.
  (define (app-cast-rule ratorT randT*)
    (cond
      [(DynT? ratorT)
       (let ([expectT* (map (const DYN) randT*)])
         (values
          (FunT (length expectT*) expectT* DYN)
          expectT*))]
      [(FunT? ratorT)
       (if (= (FunT-arity ratorT) (length randT*))
           (values ratorT (FunT-arguments ratorT))
           (raise-static-arity-mismatch ratorT randT*))]
      [else (raise-static-app-mismatch ratorT randT*)]))
  ;; typecheck all subparts
  (let*-values ([(rator ratorT) (tc/cst rator env)]
                [(rand* randT*) (tc/cst* (syntax->list rand*) env)]
                ;; apply the standard typing rule
                [(ratorT^ randT^*) (app-cast-rule ratorT randT*)]
                ;; cast everything to the expected type
                [(rator) (make-cast rator ratorT ratorT^)]
                [(rand*) (make-cast* rand* randT* randT^*)]
                ;; construct the result type and syntax
                [(appT) (FunT-return ratorT^)]
                [(app) #`(#%plain-app #,rator . #,rand*)]
                [(app) (stx-type app appT)])
    (values app appT)))

#;
(define (tc/cst-datum kwote stx)
  (let* ([dat (syntax->datum stx)]
         [ty  (or (simple-datum->type dat)
                  (raise-unsupported-datum stx))])
    (with-syntax ([stx  (stx-type stx ty)]
                  [stx^ (stx-type (quasisyntax stx (#,kwote #,stx)) ty)])
      (values #'stx^ ty))))

(define (make-cast exp t1 t2)
  exp)

(define (make-cast* exp* t1* t2*)
  (map make-cast exp* t1* t2*))

;; errors raised by typechecking use the current context for error reporting
(define current-context
  (make-parameter
   #'default-context
   (lambda (stx)
     (if (syntax? stx)
         stx
         (error 'current-context "expexted syntax got ~a" stx)))))

;; errors in typechecking and
(define (raise-unsupported-datum stx)
  (raise-syntax-error
   'type-check
   "unsupported literal"
   (current-context)
   (syntax->datum stx)))

(define (raise-static-arity-mismatch ratorT randT*)
  (raise-arity-error
   'type-check
   (FunT-arity ratorT)
   randT*))

(define (raise-static-app-mismatch ratorT randT*)
  (raise-argument-error 'application "function value" ratorT))

;; Environment Helpers
(require syntax/id-table)
(define (initial-type-environment)
  (let* ([env (make-immutable-bound-id-table)])
    env))

(define (extend env i)
  (bound-id-table-set env i (syntax-property i 'type)))

(define (lookup env i)
  (bound-id-table-ref env i))
