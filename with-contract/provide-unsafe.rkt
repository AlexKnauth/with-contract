#lang racket/base

(provide provide/contract+unsafe)

(require racket/contract/base
         (for-syntax racket/base
                     syntax/parse
                     ))

(begin-for-syntax
  (define-syntax-class prov-ctc-spec
    #:attributes (unsafe)
    [pattern [id:id ctc:expr]
             #:with unsafe #'id]
    [pattern [(~literal rename) orig:id id:id ctc:expr]
             #:with unsafe #'(rename-out [orig id])]
    [pattern [(~literal struct) struct-id:id ([field:id ctc:expr] ...) struct-option ...]
             #:with unsafe #'(struct-out struct-id)]
    ))

(define-syntax provide/contract+unsafe
  (syntax-parser
    [(provide/contract+unsafe spec:prov-ctc-spec ...)
     #'(begin
         (provide (contract-out spec ...))
         (module+ unsafe
           (provide spec.unsafe ...)))]))

