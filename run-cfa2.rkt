#lang racket
(require "../cfa2/cfa2.rkt"
         "../pda-to-pda-risc/pdarisc-node-graph.rkt"
         "../pda-to-pda-risc/node-graph.rkt"
         (rename-in "../pda-to-pda-risc/pdarisc-data.rkt"
                    (assign assign-node)
                    (assign? assign-node?)
                    (push push-node)
                    (push? push-node?)
                    (pop pop-var-rhs)
                    (pop? pop-var-rhs?)))

(provide run-cfa2)

(define-syntax time/named
  (syntax-rules ()
    ((_ name expr)
     (begin (displayln name)
            (time expr)))))

(define (run-cfa2 pda-risc #:debug [debug 0] #:max-stack-height [max-stack-height +inf.0])
  (define node-graph (time/named
                      "building node-graph"
                      (build-node-graph pda-risc)))

  ;; Min Headroom
  (define (min-headroom node-graph)
    ;; a RegisterEnv is a [Hash Symbol [SetOf Value]]
    ;; an AStack is a Symbol
    ;; an AState is a (make-abstract-state Key RegisterEnv AStack)
    (define-struct abstract-state (node re st) #:transparent)
    ;; a FlowValue is [U PositiveInteger +Infinity]
    ;; a FlowState is a (make-flow-state AState FlowValue)
    (define-struct flow-state (astate flow) #:transparent)

    ;; A SemActVal is how we represent the result of a sem-act
    (define-struct sem-act-val (name) #:transparent)


    ;; env-update : RegisterEnv RegisterName [U Value [SetOf Value]] -> RegisterEnv
    (define (env-update env reg val)
      (let ((val (if (set? val) val (set val))))
        (match reg
          ((named-reg name)
           (hash-set env (syntax-e name) val))
          ((nameless-reg)
           (hash-set env 'nameless-reg val)))))

    ;; env-ref : RegisterEnv RegisterName -> [SetOf Value]
    (define (env-ref env reg)
      (match reg
        ((named-reg name)
         (hash-ref env
                   (syntax-e name)
                   (lambda () (begin (printf "the register, ~a, is not bound yet in ~a. Returning 'bottom.\n" (syntax-e name) env)
                                     'bottom))))
        ((nameless-reg)
         (hash-ref env 'nameless-reg))))

    ;; eval-pure-rhs : pure-rhs RegisterEnv -> [SetOf Value]
    (define (eval-pure-rhs pure-rhs env)
      (match pure-rhs
        ((or (named-reg _)
             (nameless-reg))
         (env-ref env pure-rhs))
        (_ (set pure-rhs))))

    ;; push-node?/uid : Key -> Boolean
    (define (push-node?/uid uid)
      (push-node? (uid->node uid)))
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

    ;; pop-node?/uid : Key -> Boolean
    (define (pop-node?/uid uid)
      (pop-node? (uid->node uid)))

    ;; uid->node : Key -> Node
    (define (uid->node uid)
      (node-graph-get-node node-graph uid))

    ;; push-state? : FlowState -> Boolean
    (define (push-state? flow-state)
      (push-node?/uid (abstract-state-node (flow-state-astate flow-state))))
    ;; pop-state? : FlowState -> Boolean
    (define (pop-state? flow-state)
      (pop-node?/uid (abstract-state-node (flow-state-astate flow-state))))

    ;; astate-similar? : FlowState FlowState -> Boolean
    (define astate-similar? (match-lambda*
                             [(list (abstract-state node1 re1 st1)
                                    (abstract-state node2 re2 st2))
                              (and (equal? node1 node2)
                                   (equal? st1 st2))]))
    ;; flow-state-similar? : FlowState FlowState -> Boolean
    (define flow-state-similar? (match-lambda*
                                 [(list (flow-state s1 _)
                                        (flow-state s2 _))
                                  (astate-similar? s1 s2)]))
    ;; state-equal? : FlowState FlowState -> Boolean
    (define state-equal? equal?)

    ;; succ-node/uid : Key -> [SetOf Key]
    (define (succ-node/uid uid)
      (node-graph-get-succs node-graph uid))

    ;; succ-states : AState -> [SetOf AState]
    (define succ-states
      (match-lambda
       [(abstract-state uid env astack)
        (for/set ([s~ (in-set (succ-node/uid uid))])
                 (let ((node (uid->node uid)))
                   (match node
                     [(push-node pure-rhs)
                      (abstract-state s~ env (eval-pure-rhs pure-rhs env))]
                     [(sem-act name params retvars action)
                      (abstract-state s~ (env-update env
                                                     (first retvars)
                                                     (sem-act-val name))
                                      astack)]
                     [_ (abstract-state s~ env astack)])))]))

    ;; pop-succ-states : AState AState -> [SetOf AState]
    (define (pop-succ-states push pop)
      (match-define (abstract-state _ _ stack-before-push)
                    push)
      (match-define (abstract-state pop-uid env stack-after-push)
                    pop)

      (let ((reg (pop-node-reg (uid->node pop-uid))))
        (for/set ([node (in-set (succ-node/uid pop-uid))])
                 (abstract-state node
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
       [(list (abstract-state node re1 st)
              (abstract-state node re2 st))
        (abstract-state node (register-env-join re1 re2) st)]
       [(list (and a1 (abstract-state _ _ _))
              (and a2 (abstract-state _ _ _)))
        (error 'astate-join
               "States must have matching nodes and stacks. Given ~a and ~a."
               a1 a2)]))
    ;; astate-gte : AbstractState AbstractState -> Boolean
    (define astate-gte
      (match-lambda*
       [(list (abstract-state node re1 st)
              (abstract-state node re2 st))
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

      (for/set ([astate~ (in-set (succ-states astate))])
               (flow-state astate~ (next-flow fstate))))

    ;; pop-succ-states/flow : FlowState FlowState -> [SetOf FlowState]
    (define (pop-succ-states/flow push-fstate pop-fstate)
      (match-define (flow-state push-astate push-fv) push-fstate)
      (match-define (flow-state pop-astate _) pop-fstate)

      (for/set ([astate~ (in-set (pop-succ-states push-astate pop-astate))])
               (flow-state astate~ (max push-fv (next-flow pop-fstate)))))

    (FlowAnalysis (flow-state (abstract-state (node-graph-source node-graph)
                                              (hash)
                                              'ε)
                              5)
                  push-state? pop-state? state-equal?
                  flow-state-join flow-state-gte flow-state-similar?
                  succ-states/flow pop-succ-states/flow))

  (time/named
   "cfa2 min-headroom analysis"
   (CFA2 (min-headroom node-graph) #:debug debug)))