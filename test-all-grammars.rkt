#lang racket

(require racket/date
         (for-syntax racket/syntax)
         "../cfa2-results-analysis/standard-overview.rkt"
         "../cfa2-analyses/min-headroom.rkt"
         "../pda-to-pda-risc/risc-enhanced/decorate.rkt"
         "../cfa2/cfa2.rkt"
         "../cfa2-results-analysis/flow-results-to-term-results.rkt"
         "../cfa2-results-analysis/reject-dead-code.rkt")

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
                   (define pda-risc-enhanced (decorate pda-risc-name))
                   (remove-all-doomed-sequences! pda-risc-enhanced)
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
