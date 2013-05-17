#lang racket

(require "../../../../pda-to-pda-risc/macro-glue.rkt")

(provide (all-defined-out))

(define pda-risc
  (pda->pda-risc
   (TOKENS NUM PLUS *EOF*)
   (EOS *EOF*)
   (START s0)
   (RULE r1 *start () #f)
   (RULE r2 exp (NUM) NUM)
   (RULE r3 exp (PLUS exp-1 exp-2) (+ exp-1 exp-2))
   (STATE s0
          (COMMENT exp "=>" "." PLUS exp exp)
          (COMMENT exp "=>" "." NUM)
          (COMMENT *start "=>" "." exp *EOF*)
          (SHIFT (NUM) s2)
          (SHIFT (PLUS) s3)
          (GOTO exp s1))
   (STATE s1
          (COMMENT *start "=>" exp "." *EOF*)
          (ACCEPT (*EOF*)))
   (STATE s2
          (COMMENT exp "=>" NUM ".")
          (REDUCE () r2))
   (STATE s3
          (COMMENT exp "=>" PLUS "." exp exp)
          (COMMENT exp "=>" "." PLUS exp exp)
          (COMMENT exp "=>" "." NUM)
          (SHIFT (NUM) s2)
          (SHIFT (PLUS) s3)
          (GOTO exp s4))
   (STATE s4
          (COMMENT exp "=>" PLUS exp "." exp)
          (COMMENT exp "=>" "." PLUS exp exp)
          (COMMENT exp "=>" "." NUM)
          (SHIFT (NUM) s2)
          (SHIFT (PLUS) s3)
          (GOTO exp s5))
   (STATE s5
          (COMMENT exp "=>" PLUS exp exp ".")
          (REDUCE () r3))
   (STATE s6
          (COMMENT *start "=>" exp *EOF* ".")
          (REDUCE () r1))))
