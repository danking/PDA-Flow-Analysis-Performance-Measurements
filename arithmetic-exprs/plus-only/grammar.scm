(define arithmetic-plus-only
  (compile+convert-to-pda
    ((tokens NUM L-PAREN R-PAREN
             (left PLUS)
             (eos *EOF*))

     (non-term exp
	       (=> (NUM)				NUM)
	       (=> (exp PLUS exp)			(+ exp-1 exp-2))
         (=> (L-PAREN exp R-PAREN) exp)))))