#lang racket

(require "paren-pda.rkt")
(require "../../pda-to-pda-risc/risc-enhanced/data.rkt")
(require "../run-cfa2.rkt")

(provide (all-defined-out))

(define-values (node->fv Summaries Callers paren-pda-risc-enh)
  (match-let (((list node->fv Summaries Callers paren-pda-risc-enh)
               (run-cfa2 paren-pda-risc)))
    (printf "Sizes: paths: ~a, summaries: ~a, callers: ~a\n"
            (hash-count node->fv)
            (set-count Summaries)
            (set-count Callers))
    (values node->fv Summaries Callers paren-pda-risc-enh)))

(define (show)
  (unparse-pda paren-pda-risc-enh))


(require "../../cfa2/utilities.rkt")

(define (make-uid-hash fv-hash)
  (define (get-uid i)
    (cond [(insn? i) (insn-uid i)]
          [(insn*? i) (insn*-uid i)]
          [(join-point? i) (join-point-uid i)]))

  (for/hash (((k v) (in-hash fv-hash)))
    (values (get-uid (pda-term-insn k)) v)))


(define uid->fv (make-uid-hash node->fv))


(require "../../semantics/print-flow.rkt")

(define pda-risc/fvs
  (pda-risc->flow-annotated-sexp paren-pda-risc-enh
                                 (lambda (uid)
                                   (hash-ref uid->fv uid '⊥))))
