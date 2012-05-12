#lang racket
(require "../cfa2/cfa2.rkt"
         (only-in "../cfa2/utilities.rkt" bpset->fv-hash)
         (only-in "../racket-utils/similar-sets.rkt" get-basic-set)
         (rename-in "../pda-to-pda-risc/risc-enhanced/data.rkt"
                    (assign assign-node)
                    (assign? assign-node?)
                    (push push-node)
                    (push? push-node?)
                    (pop pop-var-rhs)
                    (pop? pop-var-rhs?)
                    (pda-term-succs term-succs)
                    (pda-term-insn  term->node)))

(provide run-cfa2)

(define-syntax time/named
  (syntax-rules ()
    ((_ name expr)
     (begin (displayln name)
            (time expr)))))

(define (run-cfa2 pda-risc-enh #:debug [debug 0] #:max-stack-height [max-stack-height +inf.0])
  ;; an AValue is a [SetOf Value]
  ;; a RegisterEnv is a [Hash Symbol AValue]
  (define empty-register-env (hash))
  ;; an AStack is an AValue
  ;; an AState is a (make-abstract-state Key RegisterEnv AStack)
  (define-struct abstract-state (node re st) #:transparent)
  ;; a FlowValue is [U PositiveInteger +Infinity]
  ;; a FlowState is a (make-flow-state AState FlowValue)
  (define-struct flow-state (astate flow) #:transparent)

  ;; A SemActVal is how we represent the result of a sem-act
  (define-struct sem-act-val (name) #:transparent)


  ;; Min Headroom
  (define (min-headroom)
    ;; label-closures : LabelName -> RegisterEnv
    (define label-closures (make-hasheq))

    ;; env-update : RegisterEnv RegisterName [U Value [SetOf Value]] -> RegisterEnv
    (define (env-update env reg val)
      (let ((val (if (set? val) val (set val))))
        (match reg
          ((register _ uid _ _)
           (hash-set env uid val)))))

    ;; env-ref : RegisterEnv RegisterName -> [SetOf Value]
    (define (env-ref env reg)
      (match reg
        ((register name uid _ _)
         (hash-ref env
                   uid
                   (lambda () (begin (printf "the register, ~a, is not bound yet in ~a. Returning 'bottom.\n" (syntax-e name) env)
                                     'bottom))))))

    ;; eval-pure-rhs : pure-rhs RegisterEnv -> [SetOf Value]
    (define (eval-pure-rhs pure-rhs env)
      (match pure-rhs
        ((register _ _ _ _)
         (env-ref env pure-rhs))
        (_ (set pure-rhs))))

    ;; pop-node? : Node -> Boolean
    (define (pop-node? node)
      (match node
        ((assign-node reg var-rhs)
         (pop-var-rhs? var-rhs))
        (_ #f)))
    ;; pop-node-reg : Node -> RegName
    (define (pop-node-reg pop)
      (match pop
        ((assign-node reg var-rhs)
         reg)
        (_ (error 'pop-node-reg "wasn't given a 'pop' node (an assign node)"))))

    ;; push-state? : FlowState -> Boolean
    (define (push-state? flow-state)
      (push-node? (abstract-state-node (flow-state-astate flow-state))))
    ;; pop-state? : FlowState -> Boolean
    (define (pop-state? flow-state)
      (pop-node? (abstract-state-node (flow-state-astate flow-state))))

    ;; astate-similar? : AState AState -> Boolean
    (define astate-similar? (match-lambda*
                             [(list (abstract-state term1 re1 st1)
                                    (abstract-state term2 re2 st2))
                              (and (eq? term1 term2)
                                   (equal? st1 st2))]))
    ;; astate-hash-code : AState -> Number
    (define astate-hash-code (match-lambda
                              [(abstract-state term re st)
                               (+ (equal-hash-code term)
                                  (equal-hash-code st))]))
    ;; astate-equal? : AState AState -> Boolean
    (define astate-equal? (match-lambda*
                            [(list (abstract-state term1 re1 st1)
                                   (abstract-state term2 re2 st2))
                             (and (eq? term1 term2)
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
    (define succ-states
      (match-lambda
       [(abstract-state term env astack)
        (for/seteq ([s~ (in-set (term-succs term))])
          (match (term->node term)
            [(push-node pure-rhs)
             (abstract-state s~ env (eval-pure-rhs pure-rhs env))]
            [(sem-act name params retvars action)
             (abstract-state s~ (env-update env
                                            (first retvars)
                                            (sem-act-val name))
                             astack)]
            [(go target args)
             (match-let (((join-point label params) (term->node s~)))
               (abstract-state s~
                               (for/fold ([new-env (hash-ref label-closures label)])
                                         ([parameter params]
                                          [argument args])
                                 (env-update new-env
                                             parameter
                                             (eval-pure-rhs argument
                                                            env)))
                               astack))]
            [(label ids _ _ _ _ _)
             (for ((id ids))
               (hash-set! label-closures id env))
             (abstract-state s~ env astack)]
            [_ (abstract-state s~ env astack)]))]))

    ;; pop-succ-states : AState AState -> [SetOf AState]
    (define (pop-succ-states push pop)
      (match-define (abstract-state _ _ stack-before-push)
                    push)
      (match-define (abstract-state pop-term env stack-after-push)
                    pop)

      (let ((reg (pop-node-reg (term->node pop-term))))
        (for/seteq ([term (in-set (term-succs pop-term))])
                   (abstract-state term
                                   (env-update env reg stack-after-push)
                                   stack-before-push))))

    ;; register-env-join : RegisterEnv RegisterEnv -> RegisterEnv
    (define (register-env-join re1 re2)
      (for/fold ([new-re re1])
                ([(k v) (in-hash re2)])
        (hash-set new-re k (set-union v (hash-ref new-re k (set))))))

    ;; astate-join : AbstractState AbstractState -> AbstractState
    (define astate-join
      (match-lambda*
       [(list (abstract-state t re1 st)
              (abstract-state t re2 st))
        (abstract-state t (register-env-join re1 re2) st)]
       [(list (and a1 (abstract-state _ _ _))
              (and a2 (abstract-state _ _ _)))
        (error 'astate-join
               "States must have matching nodes and stacks. Given ~a and ~a."
               a1 a2)]))
    ;; astate-gte : AbstractState AbstractState -> Boolean
    (define astate-gte
      (match-lambda*
       [(list (abstract-state t re1 st)
              (abstract-state t re2 st))
        (equal? re1 (register-env-join re1 re2))]
       [(list (abstract-state _ _ _)
              (abstract-state _ _ _))
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

      (for/seteq ([astate~ (in-set (succ-states astate))]
                  [i (set-count (succ-states astate))])
                 (when (> debug 0)
                   (printf "[succ-state/flow] ~a\n" i)
                   (flush-output))
                 (flow-state astate~ (next-flow fstate))))

    ;; pop-succ-states/flow : FlowState FlowState -> [SetOf FlowState]
    (define (pop-succ-states/flow push-fstate pop-fstate)
      (match-define (flow-state push-astate push-fv) push-fstate)
      (match-define (flow-state pop-astate _) pop-fstate)

      (for/seteq ([astate~ (in-set (pop-succ-states push-astate pop-astate))])
                 (flow-state astate~ (max push-fv (next-flow pop-fstate)))))

    (FlowAnalysis (flow-state (abstract-state (pda-risc-enh-initial-term pda-risc-enh)
                                              empty-register-env
                                              'ε)
                              5)
                  push-state? pop-state? state-equal?
                  flow-state-join flow-state-gte flow-state-similar? state-hash-code
                  succ-states/flow pop-succ-states/flow))

  (list (bpset->fv-hash (get-basic-set (time/named
                                        "cfa2 min-headroom analysis"
                                        (CFA2 (min-headroom) #:debug debug)))
                        (match-lambda [(flow-state as fv) (values as fv)])
                        min
                        +inf.0)
        pda-risc-enh))
