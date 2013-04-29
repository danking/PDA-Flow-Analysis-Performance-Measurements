#lang racket

(require "pda.rkt")
(require "../../run-cfa2.rkt")

(define-values (node->fv Summaries Callers)
  (match-let (((list node->fv Summaries Callers _)
               (run-cfa2 arithmetic-pda-risc)))
    (printf "Sizes: paths: ~a, summaries: ~a, callers: ~a\n"
            (hash-count node->fv)
            (set-count Summaries)
            (set-count Callers))
    (values node->fv Summaries Callers)))
