%token EOF SEMICOLON
    ID TYVAR
    INT  INT0
    WORD
    REAL
    STRING
    CHAR
    ABSTYPE AND
    ARROW AS BAR CASE DATATYPE DOTDOTDOT ELSE END EQUALOP
    EQTYPE EXCEPTION DO DOT DARROW FN FUN FUNCTOR HANDLE
    HASH IF IN INCLUDE INFIX INFIXR LAZY LET LOCAL NONFIX OF
    OP OPEN OVERLOAD RAISE REC SHARING SIG SIGNATURE STRUCT
    STRUCTURE THEN TYPE VAL WHERE WHILE WILD WITH WITHTYPE
    ASTERISK COLON COLONGT COMMA LBRACE LBRACKET LPAREN RBRACE
    RBRACKET RPAREN ORELSE ANDALSO FUNSIG VECTORSTART BEGINQ
    ENDQ OBJL AQID

%verbose
%pos int
%arg (error) : pos * pos -> ErrorMsg.complainer
%start interdec
%eop EOF SEMICOLON
%noshift EOF

%nonassoc WITHTYPE
%right AND
%right ARROW
%right DARROW
%left DO
%left ELSE
%left RAISE
%right HANDLE
%right ORELSE
%right ANDALSO
%right AS
%left COLON

%%

int : INT
  | INT0
;

id  : ID
  | ASTERISK
;

ident : ID
  | ASTERISK
  | EQUALOP
;

op_op : OP
  |
;

qid : ID DOT qid
  | ident
;

selector: id
  | INT
;

tycon   : ID DOT tycon
  | ID
;

tlabel  : selector COLON ty
;

tlabels : tlabel COMMA tlabels
  | tlabel
;

ty_prime  : TYVAR
  | LBRACE tlabels
     RBRACE
  | LBRACE RBRACE
  | LPAREN ty0_pc RPAREN tycon

  | LPAREN ty RPAREN
  | ty_prime tycon
  | tycon
;

tuple_ty : ty_prime ASTERISK tuple_ty
   | ty_prime ASTERISK ty_prime
;

ty  : tuple_ty
  | ty ARROW ty
  | ty_prime
;

ty0_pc  : ty COMMA ty
  | ty COMMA ty0_pc
;

match : rule
  | rule BAR match
;

rule  : pat DARROW exp
;



elabel  : selector EQUALOP exp
;

elabels : elabel COMMA elabels
  | elabel
;

exp_ps  : exp
  | exp SEMICOLON exp_ps
;

exp : exp HANDLE match
        | exp ORELSE exp
  | exp ANDALSO exp
  | exp COLON ty
  | app_exp
  | FN match
  | CASE exp OF match
  | WHILE exp DO exp
  | IF exp THEN exp ELSE exp
  | RAISE exp
;

app_exp : aexp
  | ident
  | aexp app_exp
  | ident app_exp
;

 aexp : OP ident
  | ID DOT qid
  | int
  | WORD
  | REAL
  | STRING
  | CHAR
  | HASH selector
  | LBRACE elabels RBRACE
  | LBRACE RBRACE
  | LPAREN RPAREN
  | LPAREN exp_ps RPAREN
  | LPAREN exp_2c RPAREN
  | LBRACKET exp_list RBRACKET
  | LBRACKET RBRACKET
        | VECTORSTART exp_list RBRACKET
        | VECTORSTART RBRACKET
  | LET ldecs IN exp_ps END

        | AQID
        | quote
;

quote   : BEGINQ ENDQ
        | BEGINQ ot_list ENDQ
;

ot_list : OBJL aexp
        | OBJL aexp ot_list
;

exp_2c  : exp COMMA exp_2c
  | exp COMMA exp
;

exp_list : exp
   | exp COMMA exp_list
;

pat : pat AS pat
  | pat COLON ty
  | apats
;

apat  : apat_prime
  | LPAREN pat RPAREN
  | id
  | LPAREN RPAREN
  | LPAREN pat COMMA  pat_list RPAREN

  | LPAREN pat BAR or_pat_list RPAREN

;

apat_prime  : OP ident
  | ID DOT qid
  | int
  | WORD
  | STRING
        | CHAR
  | WILD
  | LBRACKET RBRACKET
  | LBRACKET pat_list
    RBRACKET
        | VECTORSTART RBRACKET
  | VECTORSTART pat_list
    RBRACKET
  | LBRACE RBRACE
  | LBRACE plabels RBRACE
;

plabel  : selector EQUALOP pat
  | ID
  | ID AS pat
  | ID COLON ty
  | ID COLON ty AS pat
;

plabels : plabel COMMA plabels
  | plabel
  | DOTDOTDOT
;

pat_list: pat
	| pat COMMA pat_list
;

or_pat_list : pat
	    | pat BAR or_pat_list
;

vb	: vb AND vb
	| LAZY pat EQUALOP exp
	| pat EQUALOP exp
;

constraint :
	   | COLON ty
;

rvb	: id constraint EQUALOP exp

	| OP id constraint EQUALOP exp

	| rvb AND rvb
        | LAZY id constraint EQUALOP exp

        | LAZY OP id constraint EQUALOP exp

;


fb_prime	: clause
	| clause BAR fb_prime
;

fb	: fb_prime
	| LAZY fb_prime
	| fb_prime AND fb
	| LAZY fb_prime AND fb
;

apats	: apat
	| apat apats
;

clause	: apats constraint EQUALOP exp

;


tb	: tyvars ID EQUALOP ty
	| tb AND tb
;

tyvars	: TYVAR
	| LPAREN tyvar_pc RPAREN
	|
;

tyvarseq: TYVAR
	| LPAREN tyvar_pc RPAREN
;

tyvar_pc: TYVAR
	| TYVAR COMMA tyvar_pc
;

dtrepl  : ID EQUALOP DATATYPE tycon
;

dbs     : db
        | db AND dbs
;

db	: ID EQUALOP constrs
	| tyvarseq ID EQUALOP constrs
        | LAZY tyvars ID EQUALOP constrs
;

constrs : constr
	| constr BAR constrs
;

constr	: op_op ident
	| op_op ident OF ty
;

eb	: op_op ident
	| op_op ident OF ty
	| op_op ident EQUALOP qid
	| eb AND eb
;

qid_p	: qid
	| qid qid_p
;

fixity	: INFIX
	| INFIX int
	| INFIXR
	| INFIXR int
	| NONFIX
;

ldec	: VAL vb
        | VAL tyvarseq vb
	| VAL REC rvb
	| VAL REC tyvarseq rvb
	| FUN fb
	| FUN tyvarseq fb
	| TYPE tb
        | DATATYPE dtrepl
	| DATATYPE dbs
 	| DATATYPE dbs WITHTYPE tb
	| ABSTYPE dbs WITH ldecs END
	| ABSTYPE dbs WITHTYPE tb WITH ldecs END
	| EXCEPTION eb
	| OPEN qid_p
	| fixity ops
	| OVERLOAD ident COLON ty AS exp_pa

;

exp_pa	: exp
	| exp AND exp_pa
;

ldecs	:
	| ldec ldecs
	| SEMICOLON ldecs
	| LOCAL ldecs IN ldecs END ldecs

;

ops	: ident
	| ident ops
;

spec_s	:
	| spec spec_s
	| SEMICOLON spec_s
;

spec	: STRUCTURE strspec
        | FUNCTOR fctspec
	| DATATYPE dtrepl
	| DATATYPE dbs
	| DATATYPE dbs WITHTYPE tb
	| TYPE tyspec
	| EQTYPE tyspec
	| VAL valspec
	| EXCEPTION exnspec
	| SHARING sharespec
	| INCLUDE sign
	| INCLUDE ident idents
;

idents	: ident
	| ident idents
;

strspec	: strspec AND strspec
	| ident COLON sign
	| ident COLON sign EQUALOP qid

;

fctspec	: fctspec AND fctspec
	| ident fsig
;

tyspec	: tyspec AND tyspec
	| tyvars ID
        | tyvars ID EQUALOP ty
;

valspec	: valspec AND valspec
	| op_op ident COLON ty
;


exnspec : exnspec AND exnspec
	| ident
	| ident OF ty
;

sharespec: sharespec AND sharespec
	 | TYPE patheqn
	 | patheqn
;

patheqn : qid EQUALOP qid
        | qid EQUALOP patheqn
;

whspec  : whspec AND whspec
        | TYPE tyvars qid EQUALOP ty

        | qid EQUALOP qid
;

sign	: ident
	| SIG spec_s END
        | sign WHERE whspec
;

sigconstraint_op :
	| COLON sign
 	| COLONGT sign
;

fsigconstraint_op :
	| COLON ident
 	| COLONGT ident
;

sigb	: sigb AND sigb
	| ident EQUALOP sign
;

fsigb	: fsigb AND fsigb
	| ident fparamList EQUALOP sign

;

fsig	: COLON ident
	| fparamList COLON sign

;

str	: qid
	| STRUCT strdecs END

	| qid arg_fct

	| LET strdecs IN str END

        | str COLON sign

        | str COLONGT sign

;

arg_fct : LPAREN strdecs RPAREN arg_fct
	| LPAREN str RPAREN arg_fct
	| LPAREN str RPAREN
	| LPAREN strdecs RPAREN
;

strdecs	: strdec strdecs
	| SEMICOLON strdecs
	|
;

sdecs	: sdec sdecs
	| SEMICOLON sdecs
	|
;

sdecs_prime	: sdec sdecs_prime
	| sdec
;

strdec	: STRUCTURE strb
	| FUNCTOR fctb
	| LOCAL strdecs IN strdecs END
	| ldec
;

sdec	: STRUCTURE strb
	| SIGNATURE sigb
	| FUNSIG fsigb
	| FUNCTOR fctb
	| LOCAL sdecs IN sdecs END
	| ldec
;

strb	: ident sigconstraint_op EQUALOP str

	| strb AND strb
;

fparam	: ID COLON sign
	| spec_s
;

fparamList
	: LPAREN fparam	RPAREN
	| LPAREN fparam RPAREN fparamList
;

fctb	: ident fparamList sigconstraint_op EQUALOP str

	| ident fsigconstraint_op EQUALOP fct_exp

	| fctb AND fctb
;

fct_exp: qid
       | qid arg_fct

       | LET strdecs IN fct_exp END

;

interdec: sdecs_prime
	| exp
;
%%
