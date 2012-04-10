(define polish-notation-plus
  (compile+convert-to-pda
    ((tokens NUM
             (left PLUS)
             (eos *EOF*))

     (non-term exp
	       (=> (NUM)				NUM)
	       (=> (PLUS exp exp)			(+ exp-1 exp-2))))))