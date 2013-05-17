#lang racket

(require "../../pda-to-pda-risc/macro-glue.rkt")

(provide (all-defined-out))

(define pda-risc
  (pda->pda-risc
   (TOKENS L-PAREN TERM R-PAREN *EOF*)
   (EOS *EOF*)
   (START s0)
   (RULE r1 *start () #f)
   (RULE r2 matches (TERM) 0)
   (RULE r3 matches (L-PAREN m R-PAREN) (+ 1 m))
   (STATE s0
          (COMMENT matches "=>" "." L-PAREN matches R-PAREN)
          (COMMENT matches "=>" "." TERM)
          (COMMENT *start "=>" "." matches *EOF*)
          (SHIFT (L-PAREN) s2)
          (SHIFT (TERM) s3)
          (GOTO matches s1))
   (STATE s1
          (COMMENT *start "=>" matches "." *EOF*)
          (ACCEPT (*EOF*)))
   (STATE s2
          (COMMENT matches "=>" L-PAREN "." matches R-PAREN)
          (COMMENT matches "=>" "." L-PAREN matches R-PAREN)
          (COMMENT matches "=>" "." TERM)
          (SHIFT (L-PAREN) s2)
          (SHIFT (TERM) s3)
          (GOTO matches s4))
   (STATE s3
          (COMMENT matches "=>" TERM ".")
          (REDUCE () r2))
   (STATE s4
          (COMMENT matches "=>" L-PAREN matches "." R-PAREN)
          (SHIFT (R-PAREN) s5))
   (STATE s5
          (COMMENT matches "=>" L-PAREN matches R-PAREN ".")
          (REDUCE () r3))
   (STATE s6
          (COMMENT *start "=>" matches *EOF* ".")
          (REDUCE () r1))))
