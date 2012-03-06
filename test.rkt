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
;(require "ml/ml-pda.rkt")
;(build-state-graph ml-pda-risc)

;; (require "java/java-pda.rkt")
;; (build-state-graph java-pda-risc)

(define-syntax time/named
  (syntax-rules ()
    ((_ name expr)
     (begin (displayln name)
            (time expr)))))

(require "c/ansi-c-pda.rkt")
(define ansi-c-node-graph (time/named
                           "building node-graph"
                           (build-node-graph ansi-c-pda-risc)))

;; Min Headroom
(define (min-headroom node-graph)
  ;; a RegisterEnv is a [Hash Symbol PureRHS]
  ;; an AStack is a Symbol
  ;; an AState is a (make-abstract-state Key RegisterEnv AStack)
  (define-struct abstract-state (node re st) #:transparent)
  ;; a FlowValue is [U PositiveInteger +Infinity]
  ;; a FlowState is a (make-flow-state AState FlowValue)
  (define-struct flow-state (astate flow) #:transparent)

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

  ;; state-similar? : FlowState FlowState -> Boolean
  (define state-similar? (match-lambda*
                           [(list (flow-state s1 _)
                                  (flow-state s2 _))
                            (equal? s1 s2)]))
  ;; state-similar? : FlowState FlowState -> Boolean
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
             [(push-node val)
              (abstract-state s~ env val)]
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
                        (hash-set env reg stack-after-push)
                        stack-before-push))))

  ;; join : FlowState FlowState -> FlowState
  (define join
    (match-lambda*
      [(list (flow-state s1 f1) (flow-state s1 f2))
       (flow-state s1 (min f1 f2))]))

  ;; gte : FlowState FlowState -> FlowState
  (define gte
    (match-lambda*
      [(list (flow-state s1 f1) (flow-state s1 f2))
       (<= f1 f2)]
      [(list (flow-state _ _) (flow-state _ _)) #f]))

  ;; next-flow : FlowState -> FlowValue
  (define (next-flow fstate)
    (match-define (flow-state _ flow) fstate)

    (cond [(push-state? fstate) (max (sub1 flow) 0)]
          [(pop-state? fstate)  (add1 flow)]
          [else flow]))

  ;; succ-states/flow : FlowState -> [SetOf FlowState]
  (define (succ-states/flow fstate)
    (match-define (flow-state astate fv) fstate)

    (for/set ([astate~ (in-set (succ-states astate))])
      (flow-state astate~ (next-flow fstate))))

  ;; pop-succ-states/flow : FlowState FlowState -> [SetOf FlowState]
  (define (pop-succ-states/flow push-fstate pop-fstate)
    (match-define (flow-state push-astate _) push-fstate)
    (match-define (flow-state pop-astate _) pop-fstate)

    (for/set ([astate~ (in-set (pop-succ-states push-astate pop-astate))])
      (flow-state astate~ (next-flow pop-fstate))))

  (FlowAnalysis (flow-state (abstract-state (node-graph-source node-graph)
                                            (hash)
                                            'ε)
                            5)
                push-state? pop-state? state-equal?
                join gte state-similar?
                succ-states/flow pop-succ-states/flow))

(time/named
 "cfa2 min-headroom analysis"
 (CFA2 (min-headroom ansi-c-node-graph)))
