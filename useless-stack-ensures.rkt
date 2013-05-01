#lang racket

(require "../pda-to-pda-risc/risc-enhanced/search.rkt"
         "../pda-to-pda-risc/risc-enhanced/data.rkt")
(provide useless-stack-ensures)

(define (useless-stack-ensures pre uid->min-hdrm)
  (define (combine t useless-ensures)
    (let ((i (pda-term-insn t)))
      (if (not (stack-ensure? i))
          useless-ensures
          (if (<= (stack-ensure-hdrm i)
                  (uid->min-hdrm (get-uid i)))
              (cons t useless-ensures)
              useless-ensures))))

  (folding-search combine empty pre))
