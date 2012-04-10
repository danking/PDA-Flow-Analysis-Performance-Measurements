(define polish-notation-plus-minus
  (compile+convert-to-pda
    ((tokens NUM
             (left MINUS PLUS)
             (eos *EOF*))

     (non-term exp
	       (=> (NUM)				NUM)
	       (=> (PLUS exp exp)			(+ exp-1 exp-2))
         (=> (MINUS exp exp)			(- exp-1 exp-2))))))