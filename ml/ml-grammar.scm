(define ml-pda
  (compile+convert-to-pda
   ((tokens SEMICOLON ID TYVAR INT INT0 WORD REAL STRING CHAR ABSTYPE
            BAR CASE DATATYPE DOTDOTDOT END EQUALOP
            EQTYPE EXCEPTION DOT FN FUN FUNCTOR
            HASH IF IN INCLUDE INFIX INFIXR LAZY LET LOCAL NONFIX OF OP OPEN
            OVERLOAD REC SHARING SIG SIGNATURE STRUCT
            STRUCTURE THEN TYPE VAL WHERE WHILE WILD WITH
            ASTERISK COLONGT COMMA LBRACE LBRACKET LPAREN RBRACE
            RBRACKET RPAREN FUNSIG VECTORSTART BEGINQ
            ENDQ OBJL AQID

            (non WITHTYPE)
            (right AND)
            (right ARROW)
            (right DARROW)
            (left DO)
            (left ELSE)
            (left RAISE)
            (right HANDLE)
            (right ORELSE)
            (right ANDALSO)
            (right AS)
            (left COLON)

            (error *ERROR*)
            (eos EOF))
    (start interdec)
    (no-shift EOF)
    (end-of-parse EOF SEMICOLON)

    (non-term int
              (=> (INT) #f)
              (=> (INT0) #f))
    (non-term id
              (=> (ID) #f)
              (=> (ASTERISK) #f))
    (non-term ident
              (=> (ID) #f)
              (=> (ASTERISK) #f)
              (=> (EQUALOP) #f)
              )
    (non-term op_op
              (=> (OP) #f)
              (=> () #f)
              )
    (non-term qid
              (=> (ID DOT qid) #f)
              (=> (ident) #f)
              )
    (non-term selector
              (=> (id) #f)
              (=> (INT) #f)
              )
    (non-term tycon
              (=> (ID DOT tycon) #f)
              (=> (ID) #f)
              )
    (non-term tlabel
              (=> (selector COLON ty) #f)
              )
    (non-term tlabels
              (=> (tlabel COMMA tlabels) #f)
              (=> (tlabel) #f)
              )
    (non-term ty_prime
     (=> (TYVAR) #f)
     (=> (LBRACE tlabels RBRACE) #f)
     (=> (LBRACE RBRACE) #f)
     (=> (LPAREN ty0_pc RPAREN tycon) #f)
     (=> (LPAREN ty RPAREN) #f)
     (=> (ty_prime tycon) #f)
     (=> (tycon) #f)
     )
    (non-term tuple_ty
     (=> (ty_prime ASTERISK tuple_ty) #f)
     (=> (ty_prime ASTERISK ty_prime) #f)
     )
    (non-term ty
              (=> (tuple_ty) #f)
              (=> (ty ARROW ty) #f)
              (=> (ty_prime) #f)
              )
    (non-term ty0_pc
              (=> (ty COMMA ty) #f)
              (=> (ty COMMA ty0_pc) #f)
              )
    (non-term match
              (=> (rule) #f)
              (=> (rule BAR match) #f)
              )
    (non-term rule
              (=> (pat DARROW exp) #f)
              )
    (non-term elabel
              (=> (selector EQUALOP exp) #f)
              )
    (non-term elabels
              (=> (elabel COMMA elabels) #f)
              (=> (elabel) #f)
              )
    (non-term exp_ps
              (=> (exp) #f)
              (=> (exp SEMICOLON exp_ps) #f)
              )
    (non-term exp
     (=> (exp HANDLE match) #f)
     (=> (exp ORELSE exp) #f)
     (=> (exp ANDALSO exp) #f)
     (=> (exp COLON ty) #f)
     (=> (app_exp) #f)
     (=> (FN match) #f)
     (=> (CASE exp OF match) #f)
     (=> (WHILE exp DO exp) #f)
     (=> (IF exp THEN exp ELSE exp) #f)
     (=> (RAISE exp) #f)
     )
    (non-term app_exp
     (=> (aexp) #f)
     (=> (ident) #f)
     (=> (aexp app_exp) #f)
     (=> (ident app_exp) #f)
     )
    (non-term aexp
     (=> (OP ident) #f)
     (=> (ID DOT qid) #f)
     (=> (int) #f)
     (=> (WORD) #f)
     (=> (REAL) #f)
     (=> (STRING) #f)
     (=> (CHAR) #f)
     (=> (HASH selector) #f)
     (=> (LBRACE elabels RBRACE) #f)
     (=> (LBRACE RBRACE) #f)
     (=> (LPAREN RPAREN) #f)
     (=> (LPAREN exp_ps RPAREN) #f)
     (=> (LPAREN exp_2c RPAREN) #f)
     (=> (LBRACKET exp_list RBRACKET) #f)
     (=> (LBRACKET RBRACKET) #f)
     (=> (VECTORSTART exp_list RBRACKET) #f)
     (=> (VECTORSTART RBRACKET) #f)
     (=> (LET ldecs IN exp_ps END) #f)
     (=> (AQID) #f)
     (=> (quote) #f)
     )
    (non-term quote
              (=> (BEGINQ ENDQ) #f)
              (=> (BEGINQ ot_list ENDQ) #f)
              )
    (non-term ot_list
              (=> (OBJL aexp) #f)
              (=> (OBJL aexp ot_list) #f)
              )
    (non-term exp_2c
              (=> (exp COMMA exp_2c) #f)
              (=> (exp COMMA exp) #f)
              )
    (non-term exp_list
              (=> (exp) #f)
              (=> (exp COMMA exp_list) #f)
              )
    (non-term pat
              (=> (pat AS pat) #f)
              (=> (pat COLON ty) #f)
              (=> (apats) #f)
              )
    (non-term apat
     (=> (apat_prime) #f)
     (=> (LPAREN pat RPAREN) #f)
     (=> (id) #f)
     (=> (LPAREN RPAREN) #f)
     (=> (LPAREN pat COMMA pat_list RPAREN) #f)
     (=> (LPAREN pat BAR or_pat_list RPAREN) #f)
     )
    (non-term apat_prime
     (=> (OP ident) #f)
     (=> (ID DOT qid) #f)
     (=> (int) #f)
     (=> (WORD) #f)
     (=> (STRING) #f)
     (=> (CHAR) #f)
     (=> (WILD) #f)
     (=> (LBRACKET RBRACKET) #f)
     (=> (LBRACKET pat_list RBRACKET) #f)
     (=> (VECTORSTART RBRACKET) #f)
     (=> (VECTORSTART pat_list RBRACKET) #f)
     (=> (LBRACE RBRACE) #f)
     (=> (LBRACE plabels RBRACE) #f)
     )
    (non-term plabel
     (=> (selector EQUALOP pat) #f)
     (=> (ID) #f)
     (=> (ID AS pat) #f)
     (=> (ID COLON ty) #f)
     (=> (ID COLON ty AS pat) #f)
     )
    (non-term plabels
              (=> (plabel COMMA plabels) #f)
              (=> (plabel) #f)
              (=> (DOTDOTDOT) #f)
              )
    (non-term pat_list
              (=> (pat) #f)
              (=> (pat COMMA pat_list) #f)
              )
    (non-term or_pat_list
              (=> (pat) #f)
              (=> (pat BAR or_pat_list) #f)
              )
    (non-term vb
              (=> (vb AND vb) #f)
              (=> (LAZY pat EQUALOP exp) #f)
              (=> (pat EQUALOP exp) #f)
              )
    (non-term constraint
              (=> () #f)
              (=> (COLON ty) #f)
              )
    (non-term rvb
     (=> (id constraint EQUALOP exp) #f)
     (=> (OP id constraint EQUALOP exp) #f)
     (=> (rvb AND rvb) #f)
     (=> (LAZY id constraint EQUALOP exp) #f)
     (=> (LAZY OP id constraint EQUALOP exp) #f)
     )
    (non-term fb_prime
              (=> (clause) #f)
              (=> (clause BAR fb_prime) #f)
              )
    (non-term fb
     (=> (fb_prime) #f)
     (=> (LAZY fb_prime) #f)
     (=> (fb_prime AND fb) #f)
     (=> (LAZY fb_prime AND fb) #f)
     )
    (non-term apats
              (=> (apat) #f)
              (=> (apat apats) #f)
              )
    (non-term clause
              (=> (apats constraint EQUALOP exp) #f)
              )
    (non-term tb
              (=> (tyvars ID EQUALOP ty) #f)
              (=> (tb AND tb) #f)
              )
    (non-term tyvars
              (=> (TYVAR) #f)
              (=> (LPAREN tyvar_pc RPAREN) #f)
              (=> () #f)
              )
    (non-term tyvarseq
              (=> (TYVAR) #f)
              (=> (LPAREN tyvar_pc RPAREN) #f)
              )
    (non-term tyvar_pc
              (=> (TYVAR) #f)
              (=> (TYVAR COMMA tyvar_pc) #f)
              )
    (non-term dtrepl
              (=> (ID EQUALOP DATATYPE tycon) #f)
              )
    (non-term dbs
              (=> (db) #f)
              (=> (db AND dbs) #f)
              )
    (non-term db
     (=> (ID EQUALOP constrs) #f)
     (=> (tyvarseq ID EQUALOP constrs) #f)
     (=> (LAZY tyvars ID EQUALOP constrs) #f)
     )
    (non-term constrs
              (=> (constr) #f)
              (=> (constr BAR constrs) #f)
              )
    (non-term constr
              (=> (op_op ident) #f)
              (=> (op_op ident OF ty) #f)
              )
    (non-term eb
     (=> (op_op ident) #f)
     (=> (op_op ident OF ty) #f)
     (=> (op_op ident EQUALOP qid) #f)
     (=> (eb AND eb) #f)
     )
    (non-term qid_p
              (=> (qid) #f)
              (=> (qid qid_p) #f)
              )
    (non-term fixity
     (=> (INFIX) #f)
     (=> (INFIX int) #f)
     (=> (INFIXR) #f)
     (=> (INFIXR int) #f)
     (=> (NONFIX) #f)
     )
    (non-term ldec
     (=> (VAL vb) #f)
     (=> (VAL tyvarseq vb) #f)
     (=> (VAL REC rvb) #f)
     (=> (VAL REC tyvarseq rvb) #f)
     (=> (FUN fb) #f)
     (=> (FUN tyvarseq fb) #f)
     (=> (TYPE tb) #f)
     (=> (DATATYPE dtrepl) #f)
     (=> (DATATYPE dbs) #f)
     (=> (DATATYPE dbs WITHTYPE tb) #f)
     (=> (ABSTYPE dbs WITH ldecs END) #f)
     (=> (ABSTYPE dbs WITHTYPE tb WITH ldecs END) #f)
     (=> (EXCEPTION eb) #f)
     (=> (OPEN qid_p) #f)
     (=> (fixity ops) #f)
     (=> (OVERLOAD ident COLON ty AS exp_pa) #f)
     )
    (non-term exp_pa
              (=> (exp) #f)
              (=> (exp AND exp_pa) #f)
              )
    (non-term ldecs
     (=> () #f)
     (=> (ldec ldecs) #f)
     (=> (SEMICOLON ldecs) #f)
     (=> (LOCAL ldecs IN ldecs END ldecs) #f)
     )
    (non-term ops
              (=> (ident) #f)
              (=> (ident ops) #f)
              )
    (non-term spec_s
              (=> () #f)
              (=> (spec spec_s) #f)
              (=> (SEMICOLON spec_s) #f)
              )
    (non-term spec
     (=> (STRUCTURE strspec) #f)
     (=> (FUNCTOR fctspec) #f)
     (=> (DATATYPE dtrepl) #f)
     (=> (DATATYPE dbs) #f)
     (=> (DATATYPE dbs WITHTYPE tb) #f)
     (=> (TYPE tyspec) #f)
     (=> (EQTYPE tyspec) #f)
     (=> (VAL valspec) #f)
     (=> (EXCEPTION exnspec) #f)
     (=> (SHARING sharespec) #f)
     (=> (INCLUDE sign) #f)
     (=> (INCLUDE ident idents) #f)
     )
    (non-term idents
              (=> (ident) #f)
              (=> (ident idents) #f)
              )
    (non-term strspec
     (=> (strspec AND strspec) #f)
     (=> (ident COLON sign) #f)
     (=> (ident COLON sign EQUALOP qid) #f)
     )
    (non-term fctspec
              (=> (fctspec AND fctspec) #f)
              (=> (ident fsig) #f)
              )
    (non-term tyspec
     (=> (tyspec AND tyspec) #f)
     (=> (tyvars ID) #f)
     (=> (tyvars ID EQUALOP ty) #f)
     )
    (non-term valspec
              (=> (valspec AND valspec) #f)
              (=> (op_op ident COLON ty) #f)
              )
    (non-term exnspec
              (=> (exnspec AND exnspec) #f)
              (=> (ident) #f)
              (=> (ident OF ty) #f)
              )
    (non-term sharespec
     (=> (sharespec AND sharespec) #f)
     (=> (TYPE patheqn) #f)
     (=> (patheqn) #f)
     )
    (non-term patheqn
              (=> (qid EQUALOP qid) #f)
              (=> (qid EQUALOP patheqn) #f)
              )
    (non-term whspec
     (=> (whspec AND whspec) #f)
     (=> (TYPE tyvars qid EQUALOP ty) #f)
     (=> (qid EQUALOP qid) #f)
     )
    (non-term sign
              (=> (ident) #f)
              (=> (SIG spec_s END) #f)
              (=> (sign WHERE whspec) #f)
              )
    (non-term sigconstraint_op
              (=> () #f)
              (=> (COLON sign) #f)
              (=> (COLONGT sign) #f)
              )
    (non-term fsigconstraint_op
              (=> () #f)
              (=> (COLON ident) #f)
              (=> (COLONGT ident) #f)
              )
    (non-term sigb
              (=> (sigb AND sigb) #f)
              (=> (ident EQUALOP sign) #f)
              )
    (non-term fsigb
              (=> (fsigb AND fsigb) #f)
              (=> (ident fparamList EQUALOP sign) #f)
              )
    (non-term fsig
              (=> (COLON ident) #f)
              (=> (fparamList COLON sign) #f)
              )
    (non-term str
     (=> (qid) #f)
     (=> (STRUCT strdecs END) #f)
     (=> (qid arg_fct) #f)
     (=> (LET strdecs IN str END) #f)
     (=> (str COLON sign) #f)
     (=> (str COLONGT sign) #f)
     )
    (non-term arg_fct
     (=> (LPAREN strdecs RPAREN arg_fct) #f)
     (=> (LPAREN str RPAREN arg_fct) #f)
     (=> (LPAREN str RPAREN) #f)
     (=> (LPAREN strdecs RPAREN) #f)
     )
    (non-term strdecs
              (=> (strdec strdecs) #f)
              (=> (SEMICOLON strdecs) #f)
              (=> () #f)
              )
    (non-term sdecs
              (=> (sdec sdecs) #f)
              (=> (SEMICOLON sdecs) #f)
              (=> () #f)
              )
    (non-term sdecs_prime
              (=> (sdec sdecs_prime) #f)
              (=> (sdec) #f)
              )
    (non-term strdec
     (=> (STRUCTURE strb) #f)
     (=> (FUNCTOR fctb) #f)
     (=> (LOCAL strdecs IN strdecs END) #f)
     (=> (ldec) #f)
     )
    (non-term sdec
     (=> (STRUCTURE strb) #f)
     (=> (SIGNATURE sigb) #f)
     (=> (FUNSIG fsigb) #f)
     (=> (FUNCTOR fctb) #f)
     (=> (LOCAL sdecs IN sdecs END) #f)
     (=> (ldec) #f)
     )
    (non-term strb
              (=> (ident sigconstraint_op EQUALOP str) #f)
              (=> (strb AND strb) #f)
              )
    (non-term fparam
              (=> (ID COLON sign) #f)
              (=> (spec_s) #f)
              )
    (non-term fparamList
     (=> (LPAREN fparam RPAREN) #f)
     (=> (LPAREN fparam RPAREN fparamList) #f)
     )
    (non-term fctb
     (=> (ident fparamList sigconstraint_op EQUALOP str) #f)
     (=> (ident fsigconstraint_op EQUALOP fct_exp) #f)
     (=> (fctb AND fctb) #f)
     )
    (non-term fct_exp
              (=> (qid) #f)
              (=> (qid arg_fct) #f)
              (=> (LET strdecs IN fct_exp END) #f)
              )
    (non-term interdec
              (=> (sdecs_prime) #f)
              (=> (exp) #f)))))

ml-pda
