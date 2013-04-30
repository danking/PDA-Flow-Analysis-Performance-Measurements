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

(define uid->term/hash
  (for/hash (((k _) (in-hash node->fv)))
    (values (get-uid (pda-term-insn k)) k)))

(define (uid->term uid)
  (hash-ref uid->term/hash uid 'unreachable))

(define uid->fv (make-uid-hash node->fv))


(require "../../semantics/print-flow.rkt")

(define pda-risc/fvs
  (pda-risc->flow-annotated-sexp paren-pda-risc-enh
                                 (lambda (uid)
                                   (hash-ref uid->fv uid '⊥))))

(require "../push-pop-webs.rkt"
         "../../cfa2/cfa2.rkt"
         (only-in "../../semantics/abstract.rkt" abstract-state-node)
         (only-in "../../semantics/flow.rkt" flow-state-astate))

(define (BP->pair-of-uid bp)
  (match-let (((BP a b) bp))
    (list (get-uid (pda-term-insn a))
          (get-uid (pda-term-insn b)))))

(define push-pop-web/uid
  (webset-from-relation (set-map Summaries BP->pair-of-uid)))

(define (uids->terms s)
  (sequence-map uid->term s))

(define (terms->unparsed-terms s)
  (sequence-map unparse s))

(define (sequence->set s)
  (for/set ((e s)) e))

(define (make-readable-webset webset)
  (for/set ((w webset))
    (web (sequence->set (terms->unparsed-terms (uids->terms (web-pushes w))))
         (sequence->set (terms->unparsed-terms (uids->terms (web-pops w)))))))

(define push-pop-web/readable
  (make-readable-webset push-pop-web/uid))
