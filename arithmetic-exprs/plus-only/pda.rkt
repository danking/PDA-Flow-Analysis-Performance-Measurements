#lang racket

(require "../../../pda-to-pda-risc/macro-glue.rkt")

(provide (all-defined-out))

(define arithmetic-plus-only-pda-risc
  (pda->pda-risc
   (TOKENS NUM L-PAREN R-PAREN PLUS *EOF*)
   (EOS *EOF*)
   (START s0)
   (RULE r1 *start () #f)
   (RULE r2 exp (NUM) NUM)
   (RULE r3 exp (exp-1 PLUS exp-2) (+ exp-1 exp-2))
   (RULE r4 exp (L-PAREN exp R-PAREN) exp)
   (STATE s0
          (COMMENT exp "=>" "." L-PAREN exp R-PAREN)
          (COMMENT exp "=>" "." exp PLUS exp)
          (COMMENT exp "=>" "." NUM)
          (COMMENT *start "=>" "." exp *EOF*)
          (SHIFT (NUM) s2)
          (SHIFT (L-PAREN) s3)
          (GOTO exp s1))
   (STATE s1
          (COMMENT exp "=>" exp "." PLUS exp)
          (COMMENT *start "=>" exp "." *EOF*)
          (SHIFT (PLUS) s6)
          (ACCEPT (*EOF*)))
   (STATE s2
          (COMMENT exp "=>" NUM ".")
          (REDUCE () r2))
   (STATE s3
          (COMMENT exp "=>" L-PAREN "." exp R-PAREN)
          (COMMENT exp "=>" "." L-PAREN exp R-PAREN)
          (COMMENT exp "=>" "." exp PLUS exp)
          (COMMENT exp "=>" "." NUM)
          (SHIFT (NUM) s2)
          (SHIFT (L-PAREN) s3)
          (GOTO exp s4))
   (STATE s4
          (COMMENT exp "=>" L-PAREN exp "." R-PAREN)
          (COMMENT exp "=>" exp "." PLUS exp)
          (SHIFT (R-PAREN) s5)
          (SHIFT (PLUS) s6))
   (STATE s5
          (COMMENT exp "=>" L-PAREN exp R-PAREN ".")
          (REDUCE () r4))
   (STATE s6
          (COMMENT exp "=>" "." L-PAREN exp R-PAREN)
          (COMMENT exp "=>" exp PLUS "." exp)
          (COMMENT exp "=>" "." exp PLUS exp)
          (COMMENT exp "=>" "." NUM)
          (SHIFT (NUM) s2)
          (SHIFT (L-PAREN) s3)
          (GOTO exp s7))
   (STATE s7
          (COMMENT exp "=>" exp PLUS exp ".")
          (COMMENT exp "=>" exp "." PLUS exp)
          (REDUCE (R-PAREN) r3)
          (REDUCE (PLUS) r3)
          (REDUCE (*EOF*) r3))
   (STATE s8
          (COMMENT *start "=>" exp *EOF* ".")
          (REDUCE () r1))))
