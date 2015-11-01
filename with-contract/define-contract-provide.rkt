#lang racket/base

(provide define/contract/provide
         define/contract/provide+unsafe
         )

(require racket/contract/base
         "provide-unsafe.rkt"
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

(define-syntax define/contract/provide+unsafe
  (syntax-parser
    [(define/contract/provide id:id ctc:expr val:expr)
     #'(begin
         (provide/contract+unsafe [id ctc])
         (define id val))]
    [(define/contract/provide+unsafe (head:expr . args) ctc:expr body:expr ...+)
     #'(define/contract/provide+unsafe head ctc (lambda args body ...))]
    ))

