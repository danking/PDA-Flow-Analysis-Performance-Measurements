(define c-pda
  (compile+convert-to-pda
    ((tokens IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
             PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
             AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
             SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
             XOR_ASSIGN OR_ASSIGN TYPE_NAME

             TYPEDEF EXTERN STATIC AUTO REGISTER
             CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID

             STRUCT UNION ENUM ELLIPSIS
             CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

             LPAREN RPAREN LBRACK RBRACK LCURLY RCURLY

             DOT COMMA

             AMPERSAND STAR PLUS MINUS TWIDLE BANG FORWARD_SLASH

             LESS_OP GREATER_OP CARROT PIPE QMARK COLON EQUAL_SIGN SEMICOLON

             (error *ERROR*)
             (eos *EOF*))
     (start translation_unit)
     (non-term primary_expression
         (=> (IDENTIFIER) 0)
         (=> (CONSTANT) 0)
         (=> (STRING_LITERAL) 0)
         (=> (LPAREN expression RPAREN) 0))
     (non-term postfix_expression
         (=> (primary_expression) 0)
         (=> (postfix_expression LBRACK expression RBRACK) 0 )
         (=> (postfix_expression LPAREN RPAREN) 0)
         (=> (postfix_expression LPAREN argument_expression_list RPAREN) 0)
         (=> (postfix_expression DOT IDENTIFIER) 0)
         (=> (postfix_expression PTR_OP IDENTIFIER) 0)
         (=> (postfix_expression INC_OP) 0)
         (=> (postfix_expression DEC_OP) 0))
     (non-term argument_expression_list
         (=> (assignment_expression) 0)
         (=> (argument_expression_list COMMA assignment_expression) 0))
     (non-term unary_expression
         (=> (postfix_expression) 0)
         (=> (INC_OP unary_expression) 0)
         (=> (DEC_OP unary_expression) 0)
         (=> (unary_operator cast_expression) 0)
         (=> (SIZEOF unary_expression) 0)
         (=> (SIZEOF LPAREN type_name RPAREN) 0))
     (non-term unary_operator
         (=> (AMPERSAND) 0)
         (=> (STAR) 0)
         (=> (PLUS) 0)
         (=> (MINUS) 0)
         (=> (TWIDLE) 0)
         (=> (BANG) 0))
     (non-term cast_expression
         (=> (unary_expression) 0)
         (=> (LPAREN type_name RPAREN cast_expression) 0))
     (non-term multiplicative_expression
         (=> (cast_expression) 0)
         (=> (multiplicative_expression STAR cast_expression) 0)
         (=> (multiplicative_expression FORWARD_SLASH cast_expression) 0)
         (=> (multiplicative_expression AMPERSAND cast_expression) 0))
     (non-term additive_expression
          (=> (multiplicative_expression) 0)
          (=> (additive_expression PLUS multiplicative_expression) 0)
          (=> (additive_expression MINUS multiplicative_expression) 0))

     (non-term shift_expression
         (=> (additive_expression) 0)
         (=> (shift_expression LEFT_OP additive_expression) 0)
         (=> (shift_expression RIGHT_OP additive_expression) 0))

     (non-term relational_expression
         (=> (shift_expression) 0)
         (=> (relational_expression LESS_OP shift_expression) 0)
         (=> (relational_expression GREATER_OP shift_expression) 0)
         (=> (relational_expression LE_OP shift_expression) 0)
         (=> (relational_expression GE_OP shift_expression) 0))

     (non-term equality_expression
         (=> (relational_expression) 0)
         (=> (equality_expression EQ_OP relational_expression) 0)
         (=> (equality_expression NE_OP relational_expression) 0))

     (non-term and_expression
         (=> (equality_expression) 0)
         (=> (and_expression AMPERSAND equality_expression) 0))

     (non-term exclusive_or_expression
         (=> (and_expression) 0)
         (=> (exclusive_or_expression CARROT and_expression) 0))

     (non-term inclusive_or_expression
         (=> (exclusive_or_expression) 0)
         (=> (inclusive_or_expression PIPE exclusive_or_expression) 0))

     (non-term logical_and_expression
         (=> (inclusive_or_expression) 0)
         (=> (logical_and_expression AND_OP inclusive_or_expression) 0))

     (non-term logical_or_expression
         (=> (logical_and_expression) 0)
         (=> (logical_or_expression OR_OP logical_and_expression) 0))

     (non-term conditional_expression
         (=> (logical_or_expression) 0)
         (=> (logical_or_expression QMARK expression COLON conditional_expression) 0))

     (non-term assignment_expression
         (=> (conditional_expression) 0)
         (=> (unary_expression assignment_operator assignment_expression) 0))

     (non-term assignment_operator
         (=> (EQUAL_SIGN) 0)
         (=> (MUL_ASSIGN) 0)
         (=> (DIV_ASSIGN) 0)
         (=> (MOD_ASSIGN) 0)
         (=> (ADD_ASSIGN) 0)
         (=> (SUB_ASSIGN) 0)
         (=> (LEFT_ASSIGN) 0)
         (=> (RIGHT_ASSIGN) 0)
         (=> (AND_ASSIGN) 0)
         (=> (XOR_ASSIGN) 0)
         (=> (OR_ASSIGN) 0))

     (non-term expression
         (=> (assignment_expression) 0)
         (=> (expression COMMA assignment_expression) 0))

     (non-term constant_expression
         (=> (conditional_expression) 0))

     (non-term declaration
         (=> (declaration_specifiers SEMICOLON) 0)
         (=> (declaration_specifiers init_declarator_list SEMICOLON) 0))

     (non-term declaration_specifiers
         (=> (storage_class_specifier) 0)
         (=> (storage_class_specifier declaration_specifiers) 0)
         (=> (type_specifier) 0)
         (=> (type_specifier declaration_specifiers) 0)
         (=> (type_qualifier) 0)
         (=> (type_qualifier declaration_specifiers) 0))

     (non-term init_declarator_list
         (=> (init_declarator) 0)
         (=> (init_declarator_list COMMA init_declarator) 0))

     (non-term init_declarator
         (=> (declarator) 0)
         (=> (declarator EQUAL_SIGN initializer) 0))

     (non-term storage_class_specifier
         (=> (TYPEDEF) 0)
         (=> (EXTERN) 0)
         (=> (STATIC) 0)
         (=> (AUTO) 0)
         (=> (REGISTER) 0))

     (non-term type_specifier
         (=> (VOID) 0)
         (=> (CHAR) 0)
         (=> (SHORT) 0)
         (=> (INT) 0)
         (=> (LONG) 0)
         (=> (FLOAT) 0)
         (=> (DOUBLE) 0)
         (=> (SIGNED) 0)
         (=> (UNSIGNED) 0)
         (=> (struct_or_union_specifier) 0)
         (=> (enum_specifier) 0)
         (=> (TYPE_NAME) 0))

     (non-term struct_or_union_specifier
         (=> (struct_or_union IDENTIFIER LCURLY struct_declaration_list RCURLY) 0)
         (=> (struct_or_union LCURLY struct_declaration_list RCURLY) 0)
         (=> (struct_or_union IDENTIFIER) 0))

     (non-term struct_or_union
         (=> (STRUCT) 0)
         (=> (UNION) 0))

     (non-term struct_declaration_list
         (=> (struct_declaration) 0)
         (=> (struct_declaration_list struct_declaration) 0))

     (non-term struct_declaration
         (=> (specifier_qualifier_list struct_declarator_list SEMICOLON) 0))

     (non-term specifier_qualifier_list
         (=> (type_specifier specifier_qualifier_list) 0)
         (=> (type_specifier) 0)
         (=> (type_qualifier specifier_qualifier_list) 0)
         (=> (type_qualifier) 0))

     (non-term struct_declarator_list
         (=> (struct_declarator) 0)
         (=> (struct_declarator_list COMMA struct_declarator) 0))

     (non-term struct_declarator
         (=> (declarator) 0)
         (=> (COLON constant_expression) 0)
         (=> (declarator COLON constant_expression) 0))

     (non-term enum_specifier
         (=> (ENUM LCURLY enumerator_list RCURLY) 0)
         (=> (ENUM IDENTIFIER LCURLY enumerator_list RCURLY) 0)
         (=> (ENUM IDENTIFIER) 0))

     (non-term enumerator_list
         (=> (enumerator) 0)
         (=> (enumerator_list COMMA enumerator) 0))

     (non-term enumerator
         (=> (IDENTIFIER) 0)
         (=> (IDENTIFIER EQUAL_SIGN constant_expression) 0))

     (non-term type_qualifier
         (=> (CONST) 0)
         (=> (VOLATILE) 0))

     (non-term declarator
         (=> (pointer direct_declarator) 0)
         (=> (direct_declarator) 0))

     (non-term direct_declarator
         (=> (IDENTIFIER) 0)
         (=> (LPAREN declarator RPAREN) 0)
         (=> (direct_declarator LBRACK constant_expression RBRACK) 0)
         (=> (direct_declarator LBRACK RBRACK) 0)
         (=> (direct_declarator LPAREN parameter_type_list RPAREN) 0)
         (=> (direct_declarator LPAREN identifier_list RPAREN) 0)
         (=> (direct_declarator LPAREN RPAREN) 0))

     (non-term pointer
         (=> (STAR) 0)
         (=> (STAR type_qualifier_list) 0)
         (=> (STAR pointer) 0)
         (=> (STAR type_qualifier_list pointer) 0))

     (non-term type_qualifier_list
         (=> (type_qualifier) 0)
         (=> (type_qualifier_list type_qualifier) 0))

     (non-term parameter_type_list
         (=> (parameter_list) 0)
         (=> (parameter_list COMMA ELLIPSIS) 0))

     (non-term parameter_list
         (=> (parameter_declaration) 0)
         (=> (parameter_list COMMA parameter_declaration) 0))

     (non-term parameter_declaration
         (=> (declaration_specifiers declarator) 0)
         (=> (declaration_specifiers abstract_declarator) 0)
         (=> (declaration_specifiers) 0))

     (non-term identifier_list
         (=> (IDENTIFIER) 0)
         (=> (identifier_list COMMA IDENTIFIER) 0))

     (non-term type_name
         (=> (specifier_qualifier_list) 0)
         (=> (specifier_qualifier_list abstract_declarator) 0))

     (non-term abstract_declarator
         (=> (pointer) 0)
         (=> (direct_abstract_declarator) 0)
         (=> (pointer direct_abstract_declarator) 0))

     (non-term direct_abstract_declarator
         (=> (LPAREN abstract_declarator RPAREN) 0)
         (=> (LBRACK RBRACK) 0)
         (=> (LBRACK constant_expression RBRACK) 0)
         (=> (direct_abstract_declarator LBRACK RBRACK) 0)
         (=> (direct_abstract_declarator LBRACK constant_expression RBRACK) 0)
         (=> (LPAREN RPAREN) 0)
         (=> (LPAREN parameter_type_list RPAREN) 0)
         (=> (direct_abstract_declarator LPAREN RPAREN) 0)
         (=> (direct_abstract_declarator LPAREN parameter_type_list RPAREN) 0))

     (non-term initializer
         (=> (assignment_expression) 0)
         (=> (LCURLY initializer_list RCURLY) 0)
         (=> (LCURLY initializer_list COMMA RCURLY) 0))

     (non-term initializer_list
         (=> (initializer) 0)
         (=> (initializer_list COMMA initializer) 0))

     (non-term statement
         (=> (labeled_statement) 0)
         (=> (compound_statement) 0)
         (=> (expression_statement) 0)
         (=> (selection_statement) 0)
         (=> (iteration_statement) 0)
         (=> (jump_statement) 0))

     (non-term labeled_statement
         (=> (IDENTIFIER COLON statement) 0)
         (=> (CASE constant_expression COLON statement) 0)
         (=> (DEFAULT COLON statement) 0))

     (non-term compound_statement
         (=> (LCURLY RCURLY) 0)
         (=> (LCURLY statement_list RCURLY) 0)
         (=> (LCURLY declaration_list RCURLY) 0)
         (=> (LCURLY declaration_list statement_list RCURLY) 0))

     (non-term declaration_list
         (=> (declaration) 0)
         (=> (declaration_list declaration) 0))

     (non-term statement_list
         (=> (statement) 0)
         (=> (statement_list statement) 0))

     (non-term expression_statement
         (=> (SEMICOLON) 0)
         (=> (expression SEMICOLON) 0))

     (non-term selection_statement
         (=> (IF LPAREN expression RPAREN statement) 0)
         (=> (IF LPAREN expression RPAREN statement ELSE statement) 0)
         (=> (SWITCH LPAREN expression RPAREN statement) 0))

     (non-term iteration_statement
         (=> (WHILE LPAREN expression RPAREN statement) 0)
         (=> (DO statement WHILE LPAREN expression RPAREN SEMICOLON) 0)
         (=> (FOR LPAREN expression_statement expression_statement RPAREN statement) 0)
         (=> (FOR LPAREN expression_statement expression_statement expression RPAREN statement) 0))

     (non-term jump_statement
         (=> (GOTO IDENTIFIER SEMICOLON) 0)
         (=> (CONTINUE SEMICOLON) 0)
         (=> (BREAK SEMICOLON) 0)
         (=> (RETURN SEMICOLON) 0)
         (=> (RETURN expression SEMICOLON) 0))

     (non-term translation_unit
         (=> (external_declaration) 0)
         (=> (translation_unit external_declaration) 0))

     (non-term external_declaration
         (=> (function_definition) 0)
         (=> (declaration) 0))

     (non-term function_definition
         (=> (declaration_specifiers declarator declaration_list compound_statement) 0)
         (=> (declaration_specifiers declarator compound_statement) 0)
         (=> (declarator declaration_list compound_statement) 0)
         (=> (declarator compound_statement) 0)))))

c-pda
