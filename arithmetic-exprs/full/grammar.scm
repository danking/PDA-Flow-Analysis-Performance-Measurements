(define arithmetic
  (compile+convert-to-pda
    ((tokens NUM L-PAREN R-PAREN
	     (left TIMES DIVIDE)
	     (left PLUS MINUS)
	     (eos *EOF*))

     (non-term exp
	       (=> (NUM)				NUM)
	       (=> (exp PLUS exp)			(+ exp-1 exp-2))
	       (=> (exp MINUS exp)			(- exp-1 exp-2))
	       (=> ((expA exp) TIMES (expB exp))	(* expA expB))
	       (=> ((expA exp) DIVIDE exp)		(quotient expA exp))
	       (=> (L-PAREN exp R-PAREN)		exp)))))