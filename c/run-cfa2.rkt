#lang racket

(require "ansi-c-pda.rkt"
         "../run-cfa2.rkt"
         "../../cfa2-results-analysis/standard-overview.rkt")

(provide (all-defined-out))

(define cfa2-results (run-cfa2 ansi-c-pda-risc))
(define summary (standard-overview cfa2-results))
(match-define
 (results-summary uid->fv/hash
                  uid->term/hash
                  ;; analysis results
                  push-pop-web
                  useless-ensures
                  ;; modified pdarisc and specific terms
                  pda-risc-enh/se
                  pda-risc/se
                  pushes
                  pops)
 summary)
