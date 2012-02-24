(define calculator
  (parse/cfg get-token cdr token-case parse-error
    ((tokens ABSTRACT
        BOOLEAN BREAK BYTE BYVALUE
        CASE CAST CATCH CHAR CLASS CONST CONTINUE
        DEFAULT DO DOUBLE
        ELSE EXTENDS
        FINAL FINALLY FLOAT FOR FUTURE
        GENERIC GOTO
        IF IMPLEMENTS IMPORT INNER INSTANCEOF INT INTERFACE
        LONG
        NATIVE NEW JNULL
        OPERATOR OUTER
        PACKAGE PRIVATE PROTECTED PUBLIC
        REST RETURN
        SHORT STATIC SUPER SWITCH SYNCHRONIZED
        THIS THROW THROWS TRANSIENT TRY
        VAR VOID VOLATILE
        WHILE
        OP_INC OP_DEC
        OP_SHL OP_SHR OP_SHRR
        OP_GE OP_LE OP_EQ OP_NE
        OP_LAND OP_LOR
        OP_DIM
        ASS_MUL ASS_DIV ASS_MOD ASS_ADD ASS_SUB
        ASS_SHL ASS_SHR ASS_SHRR ASS_AND ASS_XOR ASS_OR
        IDENTIFIER LITERAL BOOLLIT)

     (non-term program
	       (=> (s-list)				s-list)
	       (=> (s-list exp)				(begin (display exp) (newline) (cons exp s-list)))
	       (=> (s-list *ERROR*)			(cons (if #f #f) s-list)))
     (non-term s-list
	       (=> ()					'())
	       (=> (s-list statement)			(cons statement s-list)))
     (non-term statement
	       (=> (exp SEMICOLON)			(begin (display exp) (newline) exp))
	       (=> (*ERROR* SEMICOLON)			(if #f #f)))
     (non-term exp
	       (=> (NUM)				NUM)
	       (=> (exp PLUS exp)			(+ exp-1 exp-2))
	       (=> (exp MINUS exp)			(- exp-1 exp-2))
	       (=> ((expA exp) TIMES (expB exp))	(* expA expB))
	       (=> ((expA exp) DIVIDE exp)		(quotient expA exp))
	       (=> (L-PAREN exp R-PAREN)		exp)))))
