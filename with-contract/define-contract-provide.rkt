#lang racket/base

(provide define/contract/provide
         )

(require racket/contract/base
         (for-syntax racket/base
                     syntax/parse
                     ))

(define-syntax define/contract/provide
  (syntax-parser
    [(define/contract/provide id:id ctc:expr val:expr)
     #'(begin
         (provide (contract-out [id ctc]))
         (define id val))]
    [(define/contract/provide (head:expr . args) ctc:expr body:expr ...+)
     #'(define/contract/provide head ctc (lambda args body ...))]
    ))

