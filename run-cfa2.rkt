#lang racket
(require "../cfa2/cfa2.rkt"
         "../semantics/abstract.rkt"
         "../semantics/flow.rkt"
         (only-in "../cfa2/utilities.rkt"
                  bpset->fv-hash)
         (only-in "../racket-utils/similar-sets.rkt" get-basic-set)
         "../pda-to-pda-risc/risc-enhanced/decorate.rkt"
         (rename-in "../pda-to-pda-risc/risc-enhanced/data.rkt"
                    (assign assign-insn)
                    (assign? assign-insn?)
                    (push push-insn)
                    (push? push-insn?)
                    (pop pop-var-rhs)
                    (pop? pop-var-rhs?)))

(provide run-cfa2)

(define-syntax time/named
  (syntax-rules ()
    ((_ name expr)
     (begin (displayln name)
            (time expr)))))

(define (run-cfa2 pda-risc
                  #:debug [debug 0]
                  #:max-stack-height [max-stack-height +inf.0])
  (define pda-risc-enh (decorate pda-risc))

  ;; Min Headroom
  (define (min-headroom)
    ;; pop-node? : Node -> Boolean
    (define (pop-node? node)
      (match (pda-term-insn node)
        ((assign-insn _ reg var-rhs)
         (pop-var-rhs? var-rhs))
        (_ #f)))
    ;; pop-node-reg : Node -> RegName
    (define (pop-node-reg pop)
      (match (pda-term-insn pop)
        ((assign-insn _ reg var-rhs)
         reg)
        (_ (error 'pop-node-reg "wasn't given a 'pop' node (an assign insn)"))))

    ;; push-state? : FlowState -> Boolean
    (define (push-state? flow-state)
      (push-insn? (pda-term-insn
                   (abstract-state-node
                    (flow-state-astate flow-state)))))
    ;; pop-state? : FlowState -> Boolean
    (define (pop-state? flow-state)
      (pop-node? (abstract-state-node (flow-state-astate flow-state))))

    ;; astate-similar? : AState AState -> Boolean
    (define astate-similar? (match-lambda*
                             [(list (abstract-state term1 in1 st1 tr1 re1 le1)
                                    (abstract-state term2 in2 st2 tr2 re2 le2))
                              (and (equal? term1 term2)
                                   (equal? st1 st2))]))
    ;; astate-hash-code : AState -> Number
    (define astate-hash-code (match-lambda
                              [(abstract-state term in st tr re le)
                               (+ (equal-hash-code term)
                                  (equal-hash-code st))]))
    ;; astate-equal? : AState AState -> Boolean
    (define astate-equal? (match-lambda*
                            [(list (abstract-state term1 in1 st1 tr1 re1 le1)
                                   (abstract-state term2 in2 st2 tr2 re2 le2))
                             (and (equal? term1 term2)
                                  (equal? re1 re2)
                                  (equal? st1 st2))]))
    ;; flow-state-similar? : FlowState FlowState -> Boolean
    (define flow-state-similar? (match-lambda*
                                 [(list (flow-state s1 _)
                                        (flow-state s2 _))
                                  (astate-similar? s1 s2)]))
    ;; state-hash-code : FlowState -> Number
    (define state-hash-code (match-lambda
                             [(flow-state as _) (astate-hash-code as)]))
    ;; state-equal? : FlowState FlowState -> Boolean
    (define state-equal? (match-lambda*
                           [(list (flow-state s1 _)
                                  (flow-state s2 _))
                            (astate-equal? s1 s2)]))

    ;; succ-states : AState -> [SetOf AState]
    (define succ-states abstract-step)

    ;; pop-succ-states : AState AState -> [SetOf AState]
    (define (pop-succ-states push pop)
      (abstract-step/new-stack pop (abstract-state-st push)))

    ;; register-env-join : RegisterEnv RegisterEnv -> RegisterEnv
    (define (register-env-join re1 re2)
      (for/fold ([new-re re1])
                ([(k v) (in-hash re2)])
        (hash-set new-re k (set-union v (hash-ref new-re k (set))))))

    ;; astate-join : AbstractState AbstractState -> AbstractState
    (define astate-join
      (match-lambda*
       [(list (abstract-state t in st tr1 re1 le)
              (abstract-state t in st tr2 re2 le))
        (abstract-state t
                        in
                        st
                        (set-union tr1 tr2)
                        (register-env-join re1 re2)
                        le)]
       [(list (and a1 (abstract-state t1 in1 st1 tr1 re1 le1))
              (and a2 (abstract-state t2 in2 st2 tr2 re2 le2)))
        (error 'astate-join
               (string-append "States must have matching nodes, stacks, "
                              "and label environment. Given ~v and ~v.")
               a1 a2)]))
    ;; astate-gte : AbstractState AbstractState -> Boolean
    (define astate-gte
      (match-lambda*
       [(list (abstract-state t in st tr1 re1 le)
              (abstract-state t in st tr2 re2 le))
        (and (equal? re1 (register-env-join re1 re2))
             (equal? tr1 (set-union tr1 tr2)))]
       [(list (abstract-state _ _ _ _ _ _)
              (abstract-state _ _ _ _ _ _))
        #f]))

    ;; flow-state-join : FlowState FlowState -> FlowState
    (define flow-state-join
      (match-lambda*
       [(list (flow-state s1 f1) (flow-state s2 f2))
        (flow-state (astate-join s1 s2) (min f1 f2))]))

    ;; flow-state-gte : FlowState FlowState -> Boolean
    (define flow-state-gte
      (match-lambda*
       [(list (flow-state s1 f1) (flow-state s2 f2))
        (and (astate-gte s1 s2) (<= f1 f2))]))

    ;; next-flow : FlowState -> FlowValue
    (define (next-flow fstate)
      (match-define (flow-state _ flow) fstate)

      (cond [(push-state? fstate) (max (sub1 flow) 0)]
            [(pop-state? fstate)  (min (add1 flow) max-stack-height)]
            [else flow]))

    ;; succ-states/flow : FlowState -> [SetOf FlowState]
    (define (succ-states/flow fstate)
      (match-define (flow-state astate fv) fstate)

      (for/seteq ([astate~ (in-set (succ-states astate))])
        (flow-state astate~ (next-flow fstate))))

    ;; pop-succ-states/flow : FlowState FlowState -> [SetOf FlowState]
    (define (pop-succ-states/flow push-fstate pop-fstate)
      (match-define (flow-state push-astate push-fv) push-fstate)
      (match-define (flow-state pop-astate _) pop-fstate)

      (for/seteq ([astate~ (in-set (pop-succ-states push-astate pop-astate))])
        (flow-state astate~ (max push-fv (next-flow pop-fstate)))))

    (FlowAnalysis (flow-state (init-astate (pda-risc-enh-initial-term pda-risc-enh))
                              5)
                  push-state? pop-state? state-equal?
                  flow-state-join flow-state-gte flow-state-similar? state-hash-code
                  succ-states/flow pop-succ-states/flow))

  (let-values (((Paths Summaries Callers) (time/named
                                           "cfa2 min-headroom analysis"
                                           (CFA2 (min-headroom) #:debug debug))))
    (list (bpset->fv-hash (get-basic-set Paths)
                          (match-lambda [(flow-state (abstract-state node _ _ _ _ _) fv)
                                         (values node fv)])
                          min
                          +inf.0)
          (fstate-bp-set->term-bp-set Summaries)
          (fstate-bp-set->term-bp-set Callers)
          pda-risc-enh)))
