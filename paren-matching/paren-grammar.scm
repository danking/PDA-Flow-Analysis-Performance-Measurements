(define paren-matching
  (compile+convert-to-pda
    ((tokens L-PAREN TERM R-PAREN
             (eos *EOF*))

     (non-term matches
	       (=> (TERM)				0)
	       (=> (L-PAREN (m matches) R-PAREN)		(+ 1 m))))))