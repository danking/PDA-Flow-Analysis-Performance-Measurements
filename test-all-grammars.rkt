#lang racket

(require racket/date
         (for-syntax racket/syntax)
         "run-cfa2.rkt"
         "../cfa2-results-analysis/standard-overview.rkt"
         ;; new stuff
         "../cfa2-analyses/min-headroom.rkt"
         "../pda-to-pda-risc/risc-enhanced/decorate.rkt"
         "../cfa2/cfa2.rkt"
         "../cfa2-results-analysis/flow-results-to-term-results.rkt"
         ;; not currently used
         "../cfa2-results-analysis/make-dot-graph.rkt"
         "../cfa2-results-analysis/dot-graph-data.rkt"
         "../cfa2-results-analysis/annotate-dot-graph.rkt"
         "../cfa2-results-analysis/reject-dead-code.rkt"
         "../cfa2-results-analysis/missing-push-pops.rkt")

(define-syntax (get-cfa2-statistics stx)
  (syntax-case stx ()
    ((_ path ...)
     (with-syntax
         ([(pda-file-path ...) (map (lambda (x)
                                      (string-append x "/pda.rkt"))
                                    (syntax->datum #'(path ...)))]
          [(pda-risc-name ...) (map (lambda (x)
                                      (generate-temporary x))
                                    (syntax->datum #'(path ...)))])
      #`(begin (begin
                 (require (rename-in pda-file-path [pda-risc pda-risc-name]))
                 (let ()
                   (displayln (string-append "==== " path " ===="))
                   ;; (define cfa2-results (run-cfa2 pda-risc-name))
                   (define pda-risc-enhanced (decorate pda-risc-name))
                   (define-values
                     (Paths Summaries Callers) (time
                                                (CFA2 (min-headroom-analysis
                                                       pda-risc-enhanced))))
                   (define results (flow-results->term-results Paths
                                                               Summaries
                                                               Callers
                                                               min-headroom-lattice))
                   (define log-file
                     (string-append "results/"
                                    (string-replace path "/" "-")
                                    "-"
                                    (parameterize
                                        ([date-display-format 'iso-8601])
                                      (date->string (current-date)))
                                    "-"
                                    (number->string (current-seconds))
                                    ".log"))
                   (define summary (standard-overview results
                                                      pda-risc-enhanced
                                                      log-file))
                   (void)))
               ...)))))

(get-cfa2-statistics "java"
                     "ml"
                     "c"
                     "paren-matching"
                     "line-calculator"
                     "arithmetic-exprs/full"
                     "arithmetic-exprs/polish-notation/plus-only"
                     "arithmetic-exprs/polish-notation/plus-minus"
                     "arithmetic-exprs/plus-only"
                     )
