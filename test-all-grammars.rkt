#lang racket

(require racket/date
         (for-syntax racket/syntax)
         "../cfa2-results-analysis/standard-overview.rkt"
         "../cfa2-analyses/min-headroom.rkt"
         "../cfa2-analyses/max-needed.rkt"
         "../cfa2-analyses/max-headroom.rkt"
         "../pda-to-pda-risc/risc-enhanced/decorate.rkt"
         "../cfa2/cfa2.rkt"
         "../cfa2-results-analysis/flow-results-to-term-results.rkt"
         "../cfa2-results-analysis/term-results.rkt"
         "../cfa2-results-analysis/reject-dead-code.rkt"
         ;; printing pda-risc-enh terms
         (only-in "../pda-to-pda-risc/risc-enhanced/data.rkt"
                  unparse-pda/showing-label)
         ;; hack because pretty-printing of partitioned sets doesn't quite work
         (only-in "../racket-utils/partitioned-sets.rkt" pset->set))

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
                   (define analysis
                     (cons min-headroom-analysis min-headroom-bounded-lattice)
                     #;(cons (max-headroom-analysis 10) (max-headroom-lattice 10))
                     #;(cons (max-needed-analysis 10) (max-needed-lattice 10))
                     )
                   (displayln (string-append "==== " path " ===="))
                   (define pda-risc-enhanced (decorate pda-risc-name))
                   (remove-all-doomed-sequences! pda-risc-enhanced)
                   (define-values
                     (Paths Summaries Callers) (time
                                                (CFA2 ((car analysis)
                                                       pda-risc-enhanced))))
                   (define results (flow-results->term-results Paths
                                                               Summaries
                                                               Callers
                                                               (cdr analysis)))
                   (define log-file
                     (string-append (string-replace path "/" "-")
                                    "-"
                                    (parameterize
                                        ([date-display-format 'iso-8601])
                                      (date->string (current-date)))
                                    "-"
                                    (number->string (current-seconds))
                                    ".log"))
                   (define summary (standard-overview results
                                                      pda-risc-enhanced
                                                      (string-append "results/"
                                                                     log-file)))
                   (define pops-not-in-summaries
                     (for/set ((pop-term (results-summary-pops summary))
                               #:when (not
                                       (for/or ((pp-summary (term-results-summaries
                                                             results)))
                                         (equal? pop-term (second pp-summary)))))
                       pop-term))
                   (with-output-to-file (string-append "results/missing-pops/"
                                                       log-file)
                     (lambda ()
                       (pretty-print pops-not-in-summaries)))
                   (with-output-to-file (string-append "results/pda-risc-enhanced/"
                                                       log-file)
                     (lambda ()
                       (pretty-print (unparse-pda/showing-label pda-risc-enhanced))))
                   (with-output-to-file (string-append "results/flow-results/"
                                                       log-file)
                     (lambda ()
                       (pretty-print (term-results-uid->fv/hash results))))
                   (with-output-to-file (string-append "results/abstract-paths/"
                                                       log-file)
                     (lambda ()
                       (pretty-print (pset->set Paths))))
                   (with-output-to-file (string-append "results/summaries/"
                                                       log-file)
                     (lambda ()
                       (pretty-print (pset->set Summaries))))
                   (with-output-to-file (string-append "results/callers/"
                                                       log-file)
                     (lambda ()
                       (pretty-print (pset->set Callers))))
                   (void)))
               ...)))))

(define (ensure-directory-exists dir)
 (when (file-exists? dir)
   (error 'verify-directory-exists
          (string-append "The file, "
                         dir
                         " is blocking the creation of a necessary result"
                         "-collecting directory")))
 (unless (directory-exists? dir)
   (make-directory dir)))

(ensure-directory-exists "results")
(ensure-directory-exists "results/missing-pops")
(ensure-directory-exists "results/pda-risc-enhanced")
(ensure-directory-exists "results/flow-results")
(ensure-directory-exists "results/abstract-paths")
(ensure-directory-exists "results/summaries")
(ensure-directory-exists "results/callers")

(get-cfa2-statistics "paren-matching"
                     "arithmetic-exprs/polish-notation/plus-only"
                     "arithmetic-exprs/plus-only"
                     "arithmetic-exprs/polish-notation/plus-minus"
                     "arithmetic-exprs/full"
                     "line-calculator"
                     ;; "java" ;; slow
                     ;; "ml" ;; slow
                     ;; "c"  ;; slow
                     )
