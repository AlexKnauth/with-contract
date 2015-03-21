#lang racket/base

(provide define/contract
         with-contract
         let/contract
         let*/contract
         letrec/contract
         for/fold/contract
         )

(require syntax/parse/define
         racket/contract/region
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))

(begin-for-syntax
  (define-syntax-class lc-clause
    #:attributes (norm)
    [pattern (~and clause [id:id c:expr val:expr])
             #:with new-val (syntax/loc #'val
                              (with-contract id #:result c val))
             #:with norm (syntax/loc #'clause
                           [id new-val])]
    [pattern (~and norm [id:id val:expr])]))

(define-simple-macro (define-let/contract let/contract-id let-id)
  #:with ooo (quote-syntax ...)
  #:with ooo+ (quote-syntax ...+)
  (define-syntax let/contract-id
    (lambda (stx)
      (syntax-parse stx
        [(let/contract-id (clause:lc-clause ooo) body:expr ooo+)
         (syntax/loc stx
           (let-id (clause.norm ooo) body ooo))]))))

(define-let/contract let/contract let)
(define-let/contract let*/contract let*)
(define-let/contract letrec/contract letrec)

(begin-for-syntax
  (define-syntax-class ffc-accum
    #:attributes (norm c)
    [pattern (~and accum [id:id c:expr init:expr])
             #:with new-init (syntax/loc #'init
                               (with-contract id #:result c init))
             #:with norm (syntax/loc #'accum
                           [id new-init])]
    [pattern (~and accum [id:id init:expr])
             #:with c #'any/c
             #:with norm #'accum])
  (define-syntax-class ffc-clause
    #:attributes (norm def)
    [pattern (~and clause [id:id c:expr seq:expr])
             #:with tmp (generate-temporary #'id)
             #:with norm (syntax/loc #'clause
                           [tmp seq])
             #:with def #'(define id (with-contract id #:result c tmp))]
    [pattern (~and clause [([id:id c:expr] ...) seq:expr])
             #:with (tmp ...) (generate-temporaries #'(id ...))
             #:with norm (syntax/loc #'clause
                           [(tmp ...) seq])
             #:with def #'(begin (define id (with-contract id #:result c tmp)) ...)]
    [pattern (~and clause [id:id seq:expr])
             #:with norm #'clause
             #:with def #'(begin)]
    [pattern (~and clause [(id:id ...) seq:expr])
             #:with norm #'clause
             #:with def #'(begin)])
  )

(define-syntax for/fold/contract
  (lambda (stx)
    (syntax-parse stx
      [(for/fold/contract (accum:ffc-accum ...) (clause:ffc-clause ...)
         body ... final-body:expr)
       #`(for/fold (accum.norm ...) (clause.norm ...)
           body ...
           #,(syntax/loc #'final-body
               (with-contract for/fold/contract #:results (accum.c ...) final-body)))])))

