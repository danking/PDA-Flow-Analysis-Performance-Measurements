#lang racket

(require "../../pda-to-pda-risc/macro-glue.rkt")

(provide (all-defined-out))

(define java-pda-risc
   (pda->pda-risc
    (TOKENS
     ABSTRACT
     BOOLEAN
     BREAK
     BYTE
     BYVALUE
     CASE
     CAST
     CATCH
     CHAR
     CLASS
     CONST
     CONTINUE
     DEFAULT
     DO
     DOUBLE
     ELSE
     EXTENDS
     FINAL
     FINALLY
     FLOAT
     FOR
     FUTURE
     GENERIC
     GOTO
     IF
     IMPLEMENTS
     IMPORT
     INNER
     INSTANCEOF
     INT
     INTERFACE
     LONG
     NATIVE
     NEW
     JNULL
     OPERATOR
     OUTER
     PACKAGE
     PRIVATE
     PROTECTED
     PUBLIC
     REST
     RETURN
     SHORT
     STATIC
     SUPER
     SWITCH
     SYNCHRONIZED
     THIS
     THROW
     THROWS
     TRANSIENT
     TRY
     VAR
     VOID
     VOLATILE
     WHILE
     OP_INC
     OP_DEC
     OP_SHL
     OP_SHR
     OP_SHRR
     OP_GE
     OP_LE
     OP_EQ
     OP_NE
     OP_LAND
     OP_LOR
     OP_DIM
     ASS_MUL
     ASS_DIV
     ASS_MOD
     ASS_ADD
     ASS_SUB
     ASS_SHL
     ASS_SHR
     ASS_SHRR
     ASS_AND
     ASS_XOR
     ASS_OR
     IDENTIFIER
     LITERAL
     BOOLLIT
     SEMICOLON
     COMMA
     LPAREN
     RPAREN
     LBRACK
     RBRACK
     LCURLY
     RCURLY
     EQUAL_SIGN
     DOT
     PLUS
     STAR
     FORWARD_SLASH
     MINUS
     PERCENT
     LESS_THAN
     GREATER_THAN
     COLON
     PIPE
     QMARK
     AMPERSAND
     CARROT
     BANG
     TWIDDLE
     *EOF*)
    (START s0)
    (EOS *EOF*)
    (RULE r1 *start (_ _) #f)
    (RULE r2 TypeSpecifier (TypeName) 0)
    (RULE r3 TypeSpecifier (TypeName Dims) 0)
    (RULE r4 TypeName (PrimitiveType) 0)
    (RULE r5 TypeName (QualifiedName) 0)
    (RULE r6 ClassNameList (QualifiedName) 0)
    (RULE r7 ClassNameList (ClassNameList COMMA QualifiedName) 0)
    (RULE r8 PrimitiveType (BOOLEAN) 0)
    (RULE r9 PrimitiveType (CHAR) 0)
    (RULE r10 PrimitiveType (BYTE) 0)
    (RULE r11 PrimitiveType (SHORT) 0)
    (RULE r12 PrimitiveType (INT) 0)
    (RULE r13 PrimitiveType (LONG) 0)
    (RULE r14 PrimitiveType (FLOAT) 0)
    (RULE r15 PrimitiveType (DOUBLE) 0)
    (RULE r16 PrimitiveType (VOID) 0)
    (RULE r17 SemiColons (SEMICOLON) 0)
    (RULE r18 SemiColons (SemiColons SEMICOLON) 0)
    (RULE r19 CompilationUnit (ProgramFile) 0)
    (RULE
     r20
     ProgramFile
     (PackageStatement ImportStatements TypeDeclarations)
     0)
    (RULE r21 ProgramFile (PackageStatement ImportStatements) 0)
    (RULE r22 ProgramFile (PackageStatement TypeDeclarations) 0)
    (RULE r23 ProgramFile (ImportStatements TypeDeclarations) 0)
    (RULE r24 ProgramFile (PackageStatement) 0)
    (RULE r25 ProgramFile (ImportStatements) 0)
    (RULE r26 ProgramFile (TypeDeclarations) 0)
    (RULE r27 PackageStatement (PACKAGE QualifiedName SemiColons) 0)
    (RULE r28 TypeDeclarations (TypeDeclarationOptSemi) 0)
    (RULE r29 TypeDeclarations (TypeDeclarations TypeDeclarationOptSemi) 0)
    (RULE r30 TypeDeclarationOptSemi (TypeDeclaration) 0)
    (RULE r31 TypeDeclarationOptSemi (TypeDeclaration SemiColons) 0)
    (RULE r32 ImportStatements (ImportStatement) 0)
    (RULE r33 ImportStatements (ImportStatements ImportStatement) 0)
    (RULE r34 ImportStatement (IMPORT QualifiedName SemiColons) 0)
    (RULE r35 ImportStatement (IMPORT QualifiedName DOT STAR SemiColons) 0)
    (RULE r36 QualifiedName (IDENTIFIER) 0)
    (RULE r37 QualifiedName (QualifiedName DOT IDENTIFIER) 0)
    (RULE r38 TypeDeclaration (ClassHeader LCURLY FieldDeclarations RCURLY) 0)
    (RULE r39 TypeDeclaration (ClassHeader LCURLY RCURLY) 0)
    (RULE
     r40
     ClassHeader
     (Modifiers ClassWord IDENTIFIER Extends Interfaces)
     0)
    (RULE r41 ClassHeader (Modifiers ClassWord IDENTIFIER Extends) 0)
    (RULE r42 ClassHeader (Modifiers ClassWord IDENTIFIER Interfaces) 0)
    (RULE r43 ClassHeader (ClassWord IDENTIFIER Extends Interfaces) 0)
    (RULE r44 ClassHeader (Modifiers ClassWord IDENTIFIER) 0)
    (RULE r45 ClassHeader (ClassWord IDENTIFIER Extends) 0)
    (RULE r46 ClassHeader (ClassWord IDENTIFIER Interfaces) 0)
    (RULE r47 ClassHeader (ClassWord IDENTIFIER) 0)
    (RULE r48 Modifiers (Modifier) 0)
    (RULE r49 Modifiers (Modifiers Modifier) 0)
    (RULE r50 Modifier (ABSTRACT) 0)
    (RULE r51 Modifier (FINAL) 0)
    (RULE r52 Modifier (PUBLIC) 0)
    (RULE r53 Modifier (PROTECTED) 0)
    (RULE r54 Modifier (PRIVATE) 0)
    (RULE r55 Modifier (STATIC) 0)
    (RULE r56 Modifier (TRANSIENT) 0)
    (RULE r57 Modifier (VOLATILE) 0)
    (RULE r58 Modifier (NATIVE) 0)
    (RULE r59 Modifier (SYNCHRONIZED) 0)
    (RULE r60 ClassWord (CLASS) 0)
    (RULE r61 ClassWord (INTERFACE) 0)
    (RULE r62 Interfaces (IMPLEMENTS ClassNameList) 0)
    (RULE r63 FieldDeclarations (FieldDeclarationOptSemi) 0)
    (RULE r64 FieldDeclarations (FieldDeclarations FieldDeclarationOptSemi) 0)
    (RULE r65 FieldDeclarationOptSemi (FieldDeclaration) 0)
    (RULE r66 FieldDeclarationOptSemi (FieldDeclaration SemiColons) 0)
    (RULE r67 FieldDeclaration (FieldVariableDeclaration SEMICOLON) 0)
    (RULE r68 FieldDeclaration (MethodDeclaration) 0)
    (RULE r69 FieldDeclaration (ConstructorDeclaration) 0)
    (RULE r70 FieldDeclaration (StaticInitializer) 0)
    (RULE r71 FieldDeclaration (NonStaticInitializer) 0)
    (RULE r72 FieldDeclaration (TypeDeclaration) 0)
    (RULE
     r73
     FieldVariableDeclaration
     (Modifiers TypeSpecifier VariableDeclarators)
     0)
    (RULE r74 FieldVariableDeclaration (TypeSpecifier VariableDeclarators) 0)
    (RULE r75 VariableDeclarators (VariableDeclarator) 0)
    (RULE
     r76
     VariableDeclarators
     (VariableDeclarators COMMA VariableDeclarator)
     0)
    (RULE r77 VariableDeclarator (DeclaratorName) 0)
    (RULE
     r78
     VariableDeclarator
     (DeclaratorName EQUAL_SIGN VariableInitializer)
     0)
    (RULE r79 VariableInitializer (Expression) 0)
    (RULE r80 VariableInitializer (LCURLY RCURLY) 0)
    (RULE r81 VariableInitializer (LCURLY ArrayInitializers RCURLY) 0)
    (RULE r82 ArrayInitializers (VariableInitializer) 0)
    (RULE
     r83
     ArrayInitializers
     (ArrayInitializers COMMA VariableInitializer)
     0)
    (RULE r84 ArrayInitializers (ArrayInitializers COMMA) 0)
    (RULE
     r85
     MethodDeclaration
     (Modifiers TypeSpecifier MethodDeclarator Throws MethodBody)
     0)
    (RULE
     r86
     MethodDeclaration
     (Modifiers TypeSpecifier MethodDeclarator MethodBody)
     0)
    (RULE
     r87
     MethodDeclaration
     (TypeSpecifier MethodDeclarator Throws MethodBody)
     0)
    (RULE r88 MethodDeclaration (TypeSpecifier MethodDeclarator MethodBody) 0)
    (RULE r89 MethodDeclarator (DeclaratorName LPAREN ParameterList RPAREN) 0)
    (RULE r90 MethodDeclarator (DeclaratorName LPAREN RPAREN) 0)
    (RULE r91 MethodDeclarator (MethodDeclarator OP_DIM) 0)
    (RULE r92 ParameterList (Parameter) 0)
    (RULE r93 ParameterList (ParameterList COMMA Parameter) 0)
    (RULE r94 Parameter (TypeSpecifier DeclaratorName) 0)
    (RULE r95 Parameter (FINAL TypeSpecifier DeclaratorName) 0)
    (RULE r96 DeclaratorName (IDENTIFIER) 0)
    (RULE r97 DeclaratorName (DeclaratorName OP_DIM) 0)
    (RULE r98 Throws (THROWS ClassNameList) 0)
    (RULE r99 MethodBody (Block) 0)
    (RULE r100 MethodBody (SEMICOLON) 0)
    (RULE
     r101
     ConstructorDeclaration
     (Modifiers ConstructorDeclarator Throws Block)
     0)
    (RULE
     r102
     ConstructorDeclaration
     (Modifiers ConstructorDeclarator Block)
     0)
    (RULE r103 ConstructorDeclaration (ConstructorDeclarator Throws Block) 0)
    (RULE r104 ConstructorDeclaration (ConstructorDeclarator Block) 0)
    (RULE
     r105
     ConstructorDeclarator
     (IDENTIFIER LPAREN ParameterList RPAREN)
     0)
    (RULE r106 ConstructorDeclarator (IDENTIFIER LPAREN RPAREN) 0)
    (RULE r107 StaticInitializer (STATIC Block) 0)
    (RULE r108 NonStaticInitializer (Block) 0)
    (RULE r109 Extends (EXTENDS TypeName) 0)
    (RULE r110 Extends (Extends COMMA TypeName) 0)
    (RULE r111 Block (LCURLY LocalVariableDeclarationsAndStatements RCURLY) 0)
    (RULE r112 Block (LCURLY RCURLY) 0)
    (RULE
     r113
     LocalVariableDeclarationsAndStatements
     (LocalVariableDeclarationOrStatement)
     0)
    (RULE
     r114
     LocalVariableDeclarationsAndStatements
     (LocalVariableDeclarationsAndStatements
      LocalVariableDeclarationOrStatement)
     0)
    (RULE
     r115
     LocalVariableDeclarationOrStatement
     (LocalVariableDeclarationStatement)
     0)
    (RULE r116 LocalVariableDeclarationOrStatement (Statement) 0)
    (RULE
     r117
     LocalVariableDeclarationStatement
     (TypeSpecifier VariableDeclarators SEMICOLON)
     0)
    (RULE
     r118
     LocalVariableDeclarationStatement
     (FINAL TypeSpecifier VariableDeclarators SEMICOLON)
     0)
    (RULE r119 Statement (EmptyStatement) 0)
    (RULE r120 Statement (LabelStatement) 0)
    (RULE r121 Statement (ExpressionStatement SEMICOLON) 0)
    (RULE r122 Statement (SelectionStatement) 0)
    (RULE r123 Statement (IterationStatement) 0)
    (RULE r124 Statement (JumpStatement) 0)
    (RULE r125 Statement (GuardingStatement) 0)
    (RULE r126 Statement (Block) 0)
    (RULE r127 EmptyStatement (SEMICOLON) 0)
    (RULE r128 LabelStatement (IDENTIFIER COLON) 0)
    (RULE r129 LabelStatement (CASE ConstantExpression COLON) 0)
    (RULE r130 LabelStatement (DEFAULT COLON) 0)
    (RULE r131 ExpressionStatement (Expression) 0)
    (RULE r132 SelectionStatement (IF LPAREN Expression RPAREN Statement) 0)
    (RULE
     r133
     SelectionStatement
     (IF LPAREN Expression RPAREN Statement-1 ELSE Statement-2)
     0)
    (RULE r134 SelectionStatement (SWITCH LPAREN Expression RPAREN Block) 0)
    (RULE r135 IterationStatement (WHILE LPAREN Expression RPAREN Statement) 0)
    (RULE
     r136
     IterationStatement
     (DO Statement WHILE LPAREN Expression RPAREN SEMICOLON)
     0)
    (RULE
     r137
     IterationStatement
     (FOR LPAREN ForInit ForExpr ForIncr RPAREN Statement)
     0)
    (RULE
     r138
     IterationStatement
     (FOR LPAREN ForInit ForExpr RPAREN Statement)
     0)
    (RULE r139 ForInit (ExpressionStatements SEMICOLON) 0)
    (RULE r140 ForInit (LocalVariableDeclarationStatement) 0)
    (RULE r141 ForInit (SEMICOLON) 0)
    (RULE r142 ForExpr (Expression SEMICOLON) 0)
    (RULE r143 ForExpr (SEMICOLON) 0)
    (RULE r144 ForIncr (ExpressionStatements) 0)
    (RULE r145 ExpressionStatements (ExpressionStatement) 0)
    (RULE
     r146
     ExpressionStatements
     (ExpressionStatements COMMA ExpressionStatement)
     0)
    (RULE r147 JumpStatement (BREAK IDENTIFIER SEMICOLON) 0)
    (RULE r148 JumpStatement (BREAK SEMICOLON) 0)
    (RULE r149 JumpStatement (CONTINUE IDENTIFIER SEMICOLON) 0)
    (RULE r150 JumpStatement (CONTINUE SEMICOLON) 0)
    (RULE r151 JumpStatement (RETURN Expression SEMICOLON) 0)
    (RULE r152 JumpStatement (RETURN SEMICOLON) 0)
    (RULE r153 JumpStatement (THROW Expression SEMICOLON) 0)
    (RULE
     r154
     GuardingStatement
     (SYNCHRONIZED LPAREN Expression RPAREN Statement)
     0)
    (RULE r155 GuardingStatement (TRY Block Finally) 0)
    (RULE r156 GuardingStatement (TRY Block Catches) 0)
    (RULE r157 GuardingStatement (TRY Block Catches Finally) 0)
    (RULE r158 Catches (Catch) 0)
    (RULE r159 Catches (Catches Catch) 0)
    (RULE r160 Catch (CatchHeader Block) 0)
    (RULE r161 CatchHeader (CATCH LPAREN TypeSpecifier IDENTIFIER RPAREN) 0)
    (RULE r162 CatchHeader (CATCH LPAREN TypeSpecifier RPAREN) 0)
    (RULE r163 Finally (FINALLY Block) 0)
    (RULE r164 PrimaryExpression (QualifiedName) 0)
    (RULE r165 PrimaryExpression (NotJustName) 0)
    (RULE r166 NotJustName (SpecialName) 0)
    (RULE r167 NotJustName (NewAllocationExpression) 0)
    (RULE r168 NotJustName (ComplexPrimary) 0)
    (RULE r169 ComplexPrimary (LPAREN Expression RPAREN) 0)
    (RULE r170 ComplexPrimary (ComplexPrimaryNoParenthesis) 0)
    (RULE r171 ComplexPrimaryNoParenthesis (LITERAL) 0)
    (RULE r172 ComplexPrimaryNoParenthesis (BOOLLIT) 0)
    (RULE r173 ComplexPrimaryNoParenthesis (ArrayAccess) 0)
    (RULE r174 ComplexPrimaryNoParenthesis (FieldAccess) 0)
    (RULE r175 ComplexPrimaryNoParenthesis (MethodCall) 0)
    (RULE r176 ArrayAccess (QualifiedName LBRACK Expression RBRACK) 0)
    (RULE r177 ArrayAccess (ComplexPrimary LBRACK Expression RBRACK) 0)
    (RULE r178 FieldAccess (NotJustName DOT IDENTIFIER) 0)
    (RULE r179 FieldAccess (RealPostfixExpression DOT IDENTIFIER) 0)
    (RULE r180 FieldAccess (QualifiedName DOT THIS) 0)
    (RULE r181 FieldAccess (QualifiedName DOT CLASS) 0)
    (RULE r182 FieldAccess (PrimitiveType DOT CLASS) 0)
    (RULE r183 MethodCall (MethodAccess LPAREN ArgumentList RPAREN) 0)
    (RULE r184 MethodCall (MethodAccess LPAREN RPAREN) 0)
    (RULE r185 MethodAccess (ComplexPrimaryNoParenthesis) 0)
    (RULE r186 MethodAccess (SpecialName) 0)
    (RULE r187 MethodAccess (QualifiedName) 0)
    (RULE r188 SpecialName (THIS) 0)
    (RULE r189 SpecialName (SUPER) 0)
    (RULE r190 SpecialName (JNULL) 0)
    (RULE r191 ArgumentList (Expression) 0)
    (RULE r192 ArgumentList (ArgumentList COMMA Expression) 0)
    (RULE r193 NewAllocationExpression (PlainNewAllocationExpression) 0)
    (RULE
     r194
     NewAllocationExpression
     (QualifiedName DOT PlainNewAllocationExpression)
     0)
    (RULE r195 PlainNewAllocationExpression (ArrayAllocationExpression) 0)
    (RULE r196 PlainNewAllocationExpression (ClassAllocationExpression) 0)
    (RULE
     r197
     PlainNewAllocationExpression
     (ArrayAllocationExpression LCURLY RCURLY)
     0)
    (RULE
     r198
     PlainNewAllocationExpression
     (ClassAllocationExpression LCURLY RCURLY)
     0)
    (RULE
     r199
     PlainNewAllocationExpression
     (ArrayAllocationExpression LCURLY ArrayInitializers RCURLY)
     0)
    (RULE
     r200
     PlainNewAllocationExpression
     (ClassAllocationExpression LCURLY FieldDeclarations RCURLY)
     0)
    (RULE
     r201
     ClassAllocationExpression
     (NEW TypeName LPAREN ArgumentList RPAREN)
     0)
    (RULE r202 ClassAllocationExpression (NEW TypeName LPAREN RPAREN) 0)
    (RULE r203 ArrayAllocationExpression (NEW TypeName DimExprs Dims) 0)
    (RULE r204 ArrayAllocationExpression (NEW TypeName DimExprs) 0)
    (RULE r205 ArrayAllocationExpression (NEW TypeName Dims) 0)
    (RULE r206 DimExprs (DimExpr) 0)
    (RULE r207 DimExprs (DimExprs DimExpr) 0)
    (RULE r208 DimExpr (LBRACK Expression RBRACK) 0)
    (RULE r209 Dims (OP_DIM) 0)
    (RULE r210 Dims (Dims OP_DIM) 0)
    (RULE r211 PostfixExpression (PrimaryExpression) 0)
    (RULE r212 PostfixExpression (RealPostfixExpression) 0)
    (RULE r213 RealPostfixExpression (PostfixExpression OP_INC) 0)
    (RULE r214 RealPostfixExpression (PostfixExpression OP_DEC) 0)
    (RULE r215 UnaryExpression (OP_INC UnaryExpression) 0)
    (RULE r216 UnaryExpression (OP_DEC UnaryExpression) 0)
    (RULE r217 UnaryExpression (ArithmeticUnaryOperator CastExpression) 0)
    (RULE r218 UnaryExpression (LogicalUnaryExpression) 0)
    (RULE r219 LogicalUnaryExpression (PostfixExpression) 0)
    (RULE r220 LogicalUnaryExpression (LogicalUnaryOperator UnaryExpression) 0)
    (RULE r221 LogicalUnaryOperator (TWIDDLE) 0)
    (RULE r222 LogicalUnaryOperator (BANG) 0)
    (RULE r223 ArithmeticUnaryOperator (PLUS) 0)
    (RULE r224 ArithmeticUnaryOperator (MINUS) 0)
    (RULE r225 CastExpression (UnaryExpression) 0)
    (RULE
     r226
     CastExpression
     (LPAREN PrimitiveTypeExpression RPAREN CastExpression)
     0)
    (RULE
     r227
     CastExpression
     (LPAREN ClassTypeExpression RPAREN CastExpression)
     0)
    (RULE
     r228
     CastExpression
     (LPAREN Expression RPAREN LogicalUnaryExpression)
     0)
    (RULE r229 PrimitiveTypeExpression (PrimitiveType) 0)
    (RULE r230 PrimitiveTypeExpression (PrimitiveType Dims) 0)
    (RULE r231 ClassTypeExpression (QualifiedName Dims) 0)
    (RULE r232 MultiplicativeExpression (CastExpression) 0)
    (RULE
     r233
     MultiplicativeExpression
     (MultiplicativeExpression STAR CastExpression)
     0)
    (RULE
     r234
     MultiplicativeExpression
     (MultiplicativeExpression FORWARD_SLASH CastExpression)
     0)
    (RULE
     r235
     MultiplicativeExpression
     (MultiplicativeExpression PERCENT CastExpression)
     0)
    (RULE r236 AdditiveExpression (MultiplicativeExpression) 0)
    (RULE
     r237
     AdditiveExpression
     (AdditiveExpression PLUS MultiplicativeExpression)
     0)
    (RULE
     r238
     AdditiveExpression
     (AdditiveExpression MINUS MultiplicativeExpression)
     0)
    (RULE r239 ShiftExpression (AdditiveExpression) 0)
    (RULE r240 ShiftExpression (ShiftExpression OP_SHL AdditiveExpression) 0)
    (RULE r241 ShiftExpression (ShiftExpression OP_SHR AdditiveExpression) 0)
    (RULE r242 ShiftExpression (ShiftExpression OP_SHRR AdditiveExpression) 0)
    (RULE r243 RelationalExpression (ShiftExpression) 0)
    (RULE
     r244
     RelationalExpression
     (RelationalExpression LESS_THAN ShiftExpression)
     0)
    (RULE
     r245
     RelationalExpression
     (RelationalExpression GREATER_THAN ShiftExpression)
     0)
    (RULE
     r246
     RelationalExpression
     (RelationalExpression OP_LE ShiftExpression)
     0)
    (RULE
     r247
     RelationalExpression
     (RelationalExpression OP_GE ShiftExpression)
     0)
    (RULE
     r248
     RelationalExpression
     (RelationalExpression INSTANCEOF TypeSpecifier)
     0)
    (RULE r249 EqualityExpression (RelationalExpression) 0)
    (RULE
     r250
     EqualityExpression
     (EqualityExpression OP_EQ RelationalExpression)
     0)
    (RULE
     r251
     EqualityExpression
     (EqualityExpression OP_NE RelationalExpression)
     0)
    (RULE r252 AndExpression (EqualityExpression) 0)
    (RULE r253 AndExpression (AndExpression AMPERSAND EqualityExpression) 0)
    (RULE r254 ExclusiveOrExpression (AndExpression) 0)
    (RULE
     r255
     ExclusiveOrExpression
     (ExclusiveOrExpression CARROT AndExpression)
     0)
    (RULE r256 InclusiveOrExpression (ExclusiveOrExpression) 0)
    (RULE
     r257
     InclusiveOrExpression
     (InclusiveOrExpression PIPE ExclusiveOrExpression)
     0)
    (RULE r258 ConditionalAndExpression (InclusiveOrExpression) 0)
    (RULE
     r259
     ConditionalAndExpression
     (ConditionalAndExpression OP_LAND InclusiveOrExpression)
     0)
    (RULE r260 ConditionalOrExpression (ConditionalAndExpression) 0)
    (RULE
     r261
     ConditionalOrExpression
     (ConditionalOrExpression OP_LOR ConditionalAndExpression)
     0)
    (RULE r262 ConditionalExpression (ConditionalOrExpression) 0)
    (RULE
     r263
     ConditionalExpression
     (ConditionalOrExpression QMARK Expression COLON ConditionalExpression)
     0)
    (RULE r264 AssignmentExpression (ConditionalExpression) 0)
    (RULE
     r265
     AssignmentExpression
     (UnaryExpression AssignmentOperator AssignmentExpression)
     0)
    (RULE r266 AssignmentOperator (EQUAL_SIGN) 0)
    (RULE r267 AssignmentOperator (ASS_MUL) 0)
    (RULE r268 AssignmentOperator (ASS_DIV) 0)
    (RULE r269 AssignmentOperator (ASS_MOD) 0)
    (RULE r270 AssignmentOperator (ASS_ADD) 0)
    (RULE r271 AssignmentOperator (ASS_SUB) 0)
    (RULE r272 AssignmentOperator (ASS_SHL) 0)
    (RULE r273 AssignmentOperator (ASS_SHR) 0)
    (RULE r274 AssignmentOperator (ASS_SHRR) 0)
    (RULE r275 AssignmentOperator (ASS_AND) 0)
    (RULE r276 AssignmentOperator (ASS_XOR) 0)
    (RULE r277 AssignmentOperator (ASS_OR) 0)
    (RULE r278 Expression (AssignmentExpression) 0)
    (RULE r279 ConstantExpression (ConditionalExpression) 0)
    (STATE
     s0
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      ImportStatement
      "=>"
      "."
      IMPORT
      QualifiedName
      DOT
      STAR
      SemiColons)
     (COMMENT ImportStatement "=>" "." IMPORT QualifiedName SemiColons)
     (COMMENT ImportStatements "=>" "." ImportStatements ImportStatement)
     (COMMENT ImportStatements "=>" "." ImportStatement)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration SemiColons)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration)
     (COMMENT
      TypeDeclarations
      "=>"
      "."
      TypeDeclarations
      TypeDeclarationOptSemi)
     (COMMENT TypeDeclarations "=>" "." TypeDeclarationOptSemi)
     (COMMENT PackageStatement "=>" "." PACKAGE QualifiedName SemiColons)
     (COMMENT ProgramFile "=>" "." TypeDeclarations)
     (COMMENT ProgramFile "=>" "." ImportStatements)
     (COMMENT ProgramFile "=>" "." PackageStatement)
     (COMMENT ProgramFile "=>" "." ImportStatements TypeDeclarations)
     (COMMENT ProgramFile "=>" "." PackageStatement TypeDeclarations)
     (COMMENT ProgramFile "=>" "." PackageStatement ImportStatements)
     (COMMENT
      ProgramFile
      "=>"
      "."
      PackageStatement
      ImportStatements
      TypeDeclarations)
     (COMMENT CompilationUnit "=>" "." ProgramFile)
     (COMMENT *start "=>" "." CompilationUnit *EOF*)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (CLASS) s14)
     (SHIFT (FINAL) s15)
     (SHIFT (IMPORT) s16)
     (SHIFT (INTERFACE) s17)
     (SHIFT (NATIVE) s18)
     (SHIFT (PACKAGE) s19)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOLATILE) s26)
     (GOTO CompilationUnit s1)
     (GOTO ProgramFile s2)
     (GOTO PackageStatement s3)
     (GOTO TypeDeclarations s4)
     (GOTO TypeDeclarationOptSemi s5)
     (GOTO ImportStatements s6)
     (GOTO ImportStatement s7)
     (GOTO TypeDeclaration s8)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s10)
     (GOTO Modifier s11)
     (GOTO ClassWord s12))
    (STATE s1 (COMMENT *start "=>" CompilationUnit "." *EOF*) (ACCEPT (*EOF*)))
    (STATE s2 (COMMENT CompilationUnit "=>" ProgramFile ".") (REDUCE () r19))
    (STATE
     s3
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      ImportStatement
      "=>"
      "."
      IMPORT
      QualifiedName
      DOT
      STAR
      SemiColons)
     (COMMENT ImportStatement "=>" "." IMPORT QualifiedName SemiColons)
     (COMMENT ImportStatements "=>" "." ImportStatements ImportStatement)
     (COMMENT ImportStatements "=>" "." ImportStatement)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration SemiColons)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration)
     (COMMENT
      TypeDeclarations
      "=>"
      "."
      TypeDeclarations
      TypeDeclarationOptSemi)
     (COMMENT TypeDeclarations "=>" "." TypeDeclarationOptSemi)
     (COMMENT ProgramFile "=>" PackageStatement ".")
     (COMMENT ProgramFile "=>" PackageStatement "." TypeDeclarations)
     (COMMENT ProgramFile "=>" PackageStatement "." ImportStatements)
     (COMMENT
      ProgramFile
      "=>"
      PackageStatement
      "."
      ImportStatements
      TypeDeclarations)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (CLASS) s14)
     (SHIFT (FINAL) s15)
     (SHIFT (IMPORT) s16)
     (SHIFT (INTERFACE) s17)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOLATILE) s26)
     (REDUCE (*EOF*) r24)
     (GOTO TypeDeclarations s444)
     (GOTO TypeDeclarationOptSemi s5)
     (GOTO ImportStatements s445)
     (GOTO ImportStatement s7)
     (GOTO TypeDeclaration s8)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s10)
     (GOTO Modifier s11)
     (GOTO ClassWord s12))
    (STATE
     s4
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration SemiColons)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration)
     (COMMENT
      TypeDeclarations
      "=>"
      TypeDeclarations
      "."
      TypeDeclarationOptSemi)
     (COMMENT ProgramFile "=>" TypeDeclarations ".")
     (SHIFT (ABSTRACT) s13)
     (SHIFT (CLASS) s14)
     (SHIFT (FINAL) s15)
     (SHIFT (INTERFACE) s17)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOLATILE) s26)
     (REDUCE (*EOF*) r26)
     (GOTO TypeDeclarationOptSemi s443)
     (GOTO TypeDeclaration s8)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s10)
     (GOTO Modifier s11)
     (GOTO ClassWord s12))
    (STATE
     s5
     (COMMENT TypeDeclarations "=>" TypeDeclarationOptSemi ".")
     (REDUCE () r28))
    (STATE
     s6
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      ImportStatement
      "=>"
      "."
      IMPORT
      QualifiedName
      DOT
      STAR
      SemiColons)
     (COMMENT ImportStatement "=>" "." IMPORT QualifiedName SemiColons)
     (COMMENT ImportStatements "=>" ImportStatements "." ImportStatement)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration SemiColons)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration)
     (COMMENT
      TypeDeclarations
      "=>"
      "."
      TypeDeclarations
      TypeDeclarationOptSemi)
     (COMMENT TypeDeclarations "=>" "." TypeDeclarationOptSemi)
     (COMMENT ProgramFile "=>" ImportStatements ".")
     (COMMENT ProgramFile "=>" ImportStatements "." TypeDeclarations)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (CLASS) s14)
     (SHIFT (FINAL) s15)
     (SHIFT (IMPORT) s16)
     (SHIFT (INTERFACE) s17)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOLATILE) s26)
     (REDUCE (*EOF*) r25)
     (GOTO TypeDeclarations s441)
     (GOTO TypeDeclarationOptSemi s5)
     (GOTO ImportStatement s442)
     (GOTO TypeDeclaration s8)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s10)
     (GOTO Modifier s11)
     (GOTO ClassWord s12))
    (STATE
     s7
     (COMMENT ImportStatements "=>" ImportStatement ".")
     (REDUCE () r32))
    (STATE
     s8
     (COMMENT TypeDeclarationOptSemi "=>" TypeDeclaration "." SemiColons)
     (COMMENT TypeDeclarationOptSemi "=>" TypeDeclaration ".")
     (COMMENT SemiColons "=>" "." SemiColons SEMICOLON)
     (COMMENT SemiColons "=>" "." SEMICOLON)
     (REDUCE (ABSTRACT) r30)
     (REDUCE (CLASS) r30)
     (REDUCE (FINAL) r30)
     (REDUCE (INTERFACE) r30)
     (REDUCE (NATIVE) r30)
     (REDUCE (PRIVATE) r30)
     (REDUCE (PROTECTED) r30)
     (REDUCE (PUBLIC) r30)
     (REDUCE (STATIC) r30)
     (REDUCE (SYNCHRONIZED) r30)
     (REDUCE (TRANSIENT) r30)
     (REDUCE (VOLATILE) r30)
     (SHIFT (SEMICOLON) s30)
     (REDUCE (*EOF*) r30)
     (GOTO SemiColons s440))
    (STATE
     s9
     (COMMENT TypeDeclaration "=>" ClassHeader "." LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      ClassHeader
      "."
      LCURLY
      FieldDeclarations
      RCURLY)
     (SHIFT (LCURLY) s69))
    (STATE
     s10
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" Modifiers "." Modifier)
     (COMMENT ClassHeader "=>" Modifiers "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" Modifiers "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" Modifiers "." ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      Modifiers
      "."
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (CLASS) s14)
     (SHIFT (FINAL) s15)
     (SHIFT (INTERFACE) s17)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOLATILE) s26)
     (GOTO Modifier s63)
     (GOTO ClassWord s64))
    (STATE s11 (COMMENT Modifiers "=>" Modifier ".") (REDUCE () r48))
    (STATE
     s12
     (COMMENT ClassHeader "=>" ClassWord "." IDENTIFIER)
     (COMMENT ClassHeader "=>" ClassWord "." IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" ClassWord "." IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" ClassWord "." IDENTIFIER Extends Interfaces)
     (SHIFT (IDENTIFIER) s39))
    (STATE s13 (COMMENT Modifier "=>" ABSTRACT ".") (REDUCE () r50))
    (STATE s14 (COMMENT ClassWord "=>" CLASS ".") (REDUCE () r60))
    (STATE s15 (COMMENT Modifier "=>" FINAL ".") (REDUCE () r51))
    (STATE
     s16
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT
      ImportStatement
      "=>"
      IMPORT
      "."
      QualifiedName
      DOT
      STAR
      SemiColons)
     (COMMENT ImportStatement "=>" IMPORT "." QualifiedName SemiColons)
     (SHIFT (IDENTIFIER) s28)
     (GOTO QualifiedName s34))
    (STATE s17 (COMMENT ClassWord "=>" INTERFACE ".") (REDUCE () r61))
    (STATE s18 (COMMENT Modifier "=>" NATIVE ".") (REDUCE () r58))
    (STATE
     s19
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PackageStatement "=>" PACKAGE "." QualifiedName SemiColons)
     (SHIFT (IDENTIFIER) s28)
     (GOTO QualifiedName s27))
    (STATE s20 (COMMENT Modifier "=>" PRIVATE ".") (REDUCE () r54))
    (STATE s21 (COMMENT Modifier "=>" PROTECTED ".") (REDUCE () r53))
    (STATE s22 (COMMENT Modifier "=>" PUBLIC ".") (REDUCE () r52))
    (STATE s23 (COMMENT Modifier "=>" STATIC ".") (REDUCE () r55))
    (STATE s24 (COMMENT Modifier "=>" SYNCHRONIZED ".") (REDUCE () r59))
    (STATE s25 (COMMENT Modifier "=>" TRANSIENT ".") (REDUCE () r56))
    (STATE s26 (COMMENT Modifier "=>" VOLATILE ".") (REDUCE () r57))
    (STATE
     s27
     (COMMENT QualifiedName "=>" QualifiedName "." DOT IDENTIFIER)
     (COMMENT PackageStatement "=>" PACKAGE QualifiedName "." SemiColons)
     (COMMENT SemiColons "=>" "." SemiColons SEMICOLON)
     (COMMENT SemiColons "=>" "." SEMICOLON)
     (SHIFT (SEMICOLON) s30)
     (SHIFT (DOT) s31)
     (GOTO SemiColons s29))
    (STATE s28 (COMMENT QualifiedName "=>" IDENTIFIER ".") (REDUCE () r36))
    (STATE
     s29
     (COMMENT PackageStatement "=>" PACKAGE QualifiedName SemiColons ".")
     (COMMENT SemiColons "=>" SemiColons "." SEMICOLON)
     (REDUCE (ABSTRACT) r27)
     (REDUCE (CLASS) r27)
     (REDUCE (FINAL) r27)
     (REDUCE (IMPORT) r27)
     (REDUCE (INTERFACE) r27)
     (REDUCE (NATIVE) r27)
     (REDUCE (PRIVATE) r27)
     (REDUCE (PROTECTED) r27)
     (REDUCE (PUBLIC) r27)
     (REDUCE (STATIC) r27)
     (REDUCE (SYNCHRONIZED) r27)
     (REDUCE (TRANSIENT) r27)
     (REDUCE (VOLATILE) r27)
     (SHIFT (SEMICOLON) s33)
     (REDUCE (*EOF*) r27))
    (STATE s30 (COMMENT SemiColons "=>" SEMICOLON ".") (REDUCE () r17))
    (STATE
     s31
     (COMMENT QualifiedName "=>" QualifiedName DOT "." IDENTIFIER)
     (SHIFT (IDENTIFIER) s32))
    (STATE
     s32
     (COMMENT QualifiedName "=>" QualifiedName DOT IDENTIFIER ".")
     (REDUCE () r37))
    (STATE
     s33
     (COMMENT SemiColons "=>" SemiColons SEMICOLON ".")
     (REDUCE () r18))
    (STATE
     s34
     (COMMENT QualifiedName "=>" QualifiedName "." DOT IDENTIFIER)
     (COMMENT
      ImportStatement
      "=>"
      IMPORT
      QualifiedName
      "."
      DOT
      STAR
      SemiColons)
     (COMMENT ImportStatement "=>" IMPORT QualifiedName "." SemiColons)
     (COMMENT SemiColons "=>" "." SemiColons SEMICOLON)
     (COMMENT SemiColons "=>" "." SEMICOLON)
     (SHIFT (SEMICOLON) s30)
     (SHIFT (DOT) s36)
     (GOTO SemiColons s35))
    (STATE
     s35
     (COMMENT ImportStatement "=>" IMPORT QualifiedName SemiColons ".")
     (COMMENT SemiColons "=>" SemiColons "." SEMICOLON)
     (REDUCE (ABSTRACT) r34)
     (REDUCE (CLASS) r34)
     (REDUCE (FINAL) r34)
     (REDUCE (IMPORT) r34)
     (REDUCE (INTERFACE) r34)
     (REDUCE (NATIVE) r34)
     (REDUCE (PRIVATE) r34)
     (REDUCE (PROTECTED) r34)
     (REDUCE (PUBLIC) r34)
     (REDUCE (STATIC) r34)
     (REDUCE (SYNCHRONIZED) r34)
     (REDUCE (TRANSIENT) r34)
     (REDUCE (VOLATILE) r34)
     (SHIFT (SEMICOLON) s33)
     (REDUCE (*EOF*) r34))
    (STATE
     s36
     (COMMENT QualifiedName "=>" QualifiedName DOT "." IDENTIFIER)
     (COMMENT
      ImportStatement
      "=>"
      IMPORT
      QualifiedName
      DOT
      "."
      STAR
      SemiColons)
     (SHIFT (IDENTIFIER) s32)
     (SHIFT (STAR) s37))
    (STATE
     s37
     (COMMENT
      ImportStatement
      "=>"
      IMPORT
      QualifiedName
      DOT
      STAR
      "."
      SemiColons)
     (COMMENT SemiColons "=>" "." SemiColons SEMICOLON)
     (COMMENT SemiColons "=>" "." SEMICOLON)
     (SHIFT (SEMICOLON) s30)
     (GOTO SemiColons s38))
    (STATE
     s38
     (COMMENT
      ImportStatement
      "=>"
      IMPORT
      QualifiedName
      DOT
      STAR
      SemiColons
      ".")
     (COMMENT SemiColons "=>" SemiColons "." SEMICOLON)
     (REDUCE (ABSTRACT) r35)
     (REDUCE (CLASS) r35)
     (REDUCE (FINAL) r35)
     (REDUCE (IMPORT) r35)
     (REDUCE (INTERFACE) r35)
     (REDUCE (NATIVE) r35)
     (REDUCE (PRIVATE) r35)
     (REDUCE (PROTECTED) r35)
     (REDUCE (PUBLIC) r35)
     (REDUCE (STATIC) r35)
     (REDUCE (SYNCHRONIZED) r35)
     (REDUCE (TRANSIENT) r35)
     (REDUCE (VOLATILE) r35)
     (SHIFT (SEMICOLON) s33)
     (REDUCE (*EOF*) r35))
    (STATE
     s39
     (COMMENT Extends "=>" "." Extends COMMA TypeName)
     (COMMENT Extends "=>" "." EXTENDS TypeName)
     (COMMENT Interfaces "=>" "." IMPLEMENTS ClassNameList)
     (COMMENT ClassHeader "=>" ClassWord IDENTIFIER ".")
     (COMMENT ClassHeader "=>" ClassWord IDENTIFIER "." Interfaces)
     (COMMENT ClassHeader "=>" ClassWord IDENTIFIER "." Extends)
     (COMMENT ClassHeader "=>" ClassWord IDENTIFIER "." Extends Interfaces)
     (SHIFT (EXTENDS) s42)
     (SHIFT (IMPLEMENTS) s43)
     (REDUCE (LCURLY) r47)
     (GOTO Interfaces s40)
     (GOTO Extends s41))
    (STATE
     s40
     (COMMENT ClassHeader "=>" ClassWord IDENTIFIER Interfaces ".")
     (REDUCE () r46))
    (STATE
     s41
     (COMMENT Extends "=>" Extends "." COMMA TypeName)
     (COMMENT Interfaces "=>" "." IMPLEMENTS ClassNameList)
     (COMMENT ClassHeader "=>" ClassWord IDENTIFIER Extends ".")
     (COMMENT ClassHeader "=>" ClassWord IDENTIFIER Extends "." Interfaces)
     (SHIFT (IMPLEMENTS) s43)
     (SHIFT (COMMA) s61)
     (REDUCE (LCURLY) r45)
     (GOTO Interfaces s60))
    (STATE
     s42
     (COMMENT Extends "=>" EXTENDS "." TypeName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (GOTO TypeName s48)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50))
    (STATE
     s43
     (COMMENT Interfaces "=>" IMPLEMENTS "." ClassNameList)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT ClassNameList "=>" "." ClassNameList COMMA QualifiedName)
     (COMMENT ClassNameList "=>" "." QualifiedName)
     (SHIFT (IDENTIFIER) s28)
     (GOTO ClassNameList s44)
     (GOTO QualifiedName s45))
    (STATE
     s44
     (COMMENT Interfaces "=>" IMPLEMENTS ClassNameList ".")
     (COMMENT ClassNameList "=>" ClassNameList "." COMMA QualifiedName)
     (SHIFT (COMMA) s46)
     (REDUCE (LCURLY) r62))
    (STATE
     s45
     (COMMENT QualifiedName "=>" QualifiedName "." DOT IDENTIFIER)
     (COMMENT ClassNameList "=>" QualifiedName ".")
     (REDUCE (SEMICOLON) r6)
     (REDUCE (COMMA) r6)
     (REDUCE (LCURLY) r6)
     (SHIFT (DOT) s31))
    (STATE
     s46
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT ClassNameList "=>" ClassNameList COMMA "." QualifiedName)
     (SHIFT (IDENTIFIER) s28)
     (GOTO QualifiedName s47))
    (STATE
     s47
     (COMMENT QualifiedName "=>" QualifiedName "." DOT IDENTIFIER)
     (COMMENT ClassNameList "=>" ClassNameList COMMA QualifiedName ".")
     (REDUCE (SEMICOLON) r7)
     (REDUCE (COMMA) r7)
     (REDUCE (LCURLY) r7)
     (SHIFT (DOT) s31))
    (STATE s48 (COMMENT Extends "=>" EXTENDS TypeName ".") (REDUCE () r109))
    (STATE s49 (COMMENT TypeName "=>" PrimitiveType ".") (REDUCE () r4))
    (STATE
     s50
     (COMMENT QualifiedName "=>" QualifiedName "." DOT IDENTIFIER)
     (COMMENT TypeName "=>" QualifiedName ".")
     (REDUCE (IMPLEMENTS) r5)
     (REDUCE (INSTANCEOF) r5)
     (REDUCE (OP_GE) r5)
     (REDUCE (OP_LE) r5)
     (REDUCE (OP_EQ) r5)
     (REDUCE (OP_NE) r5)
     (REDUCE (OP_LAND) r5)
     (REDUCE (OP_LOR) r5)
     (REDUCE (OP_DIM) r5)
     (REDUCE (IDENTIFIER) r5)
     (REDUCE (SEMICOLON) r5)
     (REDUCE (COMMA) r5)
     (REDUCE (LPAREN) r5)
     (REDUCE (RPAREN) r5)
     (REDUCE (LBRACK) r5)
     (REDUCE (RBRACK) r5)
     (REDUCE (LCURLY) r5)
     (REDUCE (RCURLY) r5)
     (SHIFT (DOT) s31)
     (REDUCE (LESS_THAN) r5)
     (REDUCE (GREATER_THAN) r5)
     (REDUCE (COLON) r5)
     (REDUCE (PIPE) r5)
     (REDUCE (QMARK) r5)
     (REDUCE (AMPERSAND) r5)
     (REDUCE (CARROT) r5))
    (STATE s51 (COMMENT PrimitiveType "=>" BOOLEAN ".") (REDUCE () r8))
    (STATE s52 (COMMENT PrimitiveType "=>" BYTE ".") (REDUCE () r10))
    (STATE s53 (COMMENT PrimitiveType "=>" CHAR ".") (REDUCE () r9))
    (STATE s54 (COMMENT PrimitiveType "=>" DOUBLE ".") (REDUCE () r15))
    (STATE s55 (COMMENT PrimitiveType "=>" FLOAT ".") (REDUCE () r14))
    (STATE s56 (COMMENT PrimitiveType "=>" INT ".") (REDUCE () r12))
    (STATE s57 (COMMENT PrimitiveType "=>" LONG ".") (REDUCE () r13))
    (STATE s58 (COMMENT PrimitiveType "=>" SHORT ".") (REDUCE () r11))
    (STATE s59 (COMMENT PrimitiveType "=>" VOID ".") (REDUCE () r16))
    (STATE
     s60
     (COMMENT ClassHeader "=>" ClassWord IDENTIFIER Extends Interfaces ".")
     (REDUCE () r43))
    (STATE
     s61
     (COMMENT Extends "=>" Extends COMMA "." TypeName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (GOTO TypeName s62)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50))
    (STATE
     s62
     (COMMENT Extends "=>" Extends COMMA TypeName ".")
     (REDUCE () r110))
    (STATE s63 (COMMENT Modifiers "=>" Modifiers Modifier ".") (REDUCE () r49))
    (STATE
     s64
     (COMMENT ClassHeader "=>" Modifiers ClassWord "." IDENTIFIER)
     (COMMENT ClassHeader "=>" Modifiers ClassWord "." IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" Modifiers ClassWord "." IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      Modifiers
      ClassWord
      "."
      IDENTIFIER
      Extends
      Interfaces)
     (SHIFT (IDENTIFIER) s65))
    (STATE
     s65
     (COMMENT Extends "=>" "." Extends COMMA TypeName)
     (COMMENT Extends "=>" "." EXTENDS TypeName)
     (COMMENT Interfaces "=>" "." IMPLEMENTS ClassNameList)
     (COMMENT ClassHeader "=>" Modifiers ClassWord IDENTIFIER ".")
     (COMMENT ClassHeader "=>" Modifiers ClassWord IDENTIFIER "." Interfaces)
     (COMMENT ClassHeader "=>" Modifiers ClassWord IDENTIFIER "." Extends)
     (COMMENT
      ClassHeader
      "=>"
      Modifiers
      ClassWord
      IDENTIFIER
      "."
      Extends
      Interfaces)
     (SHIFT (EXTENDS) s42)
     (SHIFT (IMPLEMENTS) s43)
     (REDUCE (LCURLY) r44)
     (GOTO Interfaces s66)
     (GOTO Extends s67))
    (STATE
     s66
     (COMMENT ClassHeader "=>" Modifiers ClassWord IDENTIFIER Interfaces ".")
     (REDUCE () r42))
    (STATE
     s67
     (COMMENT Extends "=>" Extends "." COMMA TypeName)
     (COMMENT Interfaces "=>" "." IMPLEMENTS ClassNameList)
     (COMMENT ClassHeader "=>" Modifiers ClassWord IDENTIFIER Extends ".")
     (COMMENT
      ClassHeader
      "=>"
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      "."
      Interfaces)
     (SHIFT (IMPLEMENTS) s43)
     (SHIFT (COMMA) s61)
     (REDUCE (LCURLY) r41)
     (GOTO Interfaces s68))
    (STATE
     s68
     (COMMENT
      ClassHeader
      "=>"
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces
      ".")
     (REDUCE () r40))
    (STATE
     s69
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT NonStaticInitializer "=>" "." Block)
     (COMMENT StaticInitializer "=>" "." STATIC Block)
     (COMMENT ConstructorDeclarator "=>" "." IDENTIFIER LPAREN RPAREN)
     (COMMENT
      ConstructorDeclarator
      "=>"
      "."
      IDENTIFIER
      LPAREN
      ParameterList
      RPAREN)
     (COMMENT ConstructorDeclaration "=>" "." ConstructorDeclarator Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      ConstructorDeclarator
      Throws
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      Modifiers
      ConstructorDeclarator
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      Modifiers
      ConstructorDeclarator
      Throws
      Block)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      TypeSpecifier
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      "."
      TypeSpecifier
      VariableDeclarators)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      VariableDeclarators)
     (COMMENT FieldDeclaration "=>" "." TypeDeclaration)
     (COMMENT FieldDeclaration "=>" "." NonStaticInitializer)
     (COMMENT FieldDeclaration "=>" "." StaticInitializer)
     (COMMENT FieldDeclaration "=>" "." ConstructorDeclaration)
     (COMMENT FieldDeclaration "=>" "." MethodDeclaration)
     (COMMENT FieldDeclaration "=>" "." FieldVariableDeclaration SEMICOLON)
     (COMMENT FieldDeclarationOptSemi "=>" "." FieldDeclaration SemiColons)
     (COMMENT FieldDeclarationOptSemi "=>" "." FieldDeclaration)
     (COMMENT
      FieldDeclarations
      "=>"
      "."
      FieldDeclarations
      FieldDeclarationOptSemi)
     (COMMENT FieldDeclarations "=>" "." FieldDeclarationOptSemi)
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" ClassHeader LCURLY "." RCURLY)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      ClassHeader
      LCURLY
      "."
      FieldDeclarations
      RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (CLASS) s14)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s15)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (INTERFACE) s17)
     (SHIFT (LONG) s57)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (SHORT) s58)
     (SHIFT (STATIC) s84)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOID) s59)
     (SHIFT (VOLATILE) s26)
     (SHIFT (IDENTIFIER) s85)
     (SHIFT (LCURLY) s86)
     (SHIFT (RCURLY) s87)
     (GOTO TypeSpecifier s70)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50)
     (GOTO TypeDeclaration s72)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s73)
     (GOTO Modifier s11)
     (GOTO ClassWord s12)
     (GOTO FieldDeclarations s74)
     (GOTO FieldDeclarationOptSemi s75)
     (GOTO FieldDeclaration s76)
     (GOTO FieldVariableDeclaration s77)
     (GOTO MethodDeclaration s78)
     (GOTO ConstructorDeclaration s79)
     (GOTO ConstructorDeclarator s80)
     (GOTO StaticInitializer s81)
     (GOTO NonStaticInitializer s82)
     (GOTO Block s83))
    (STATE
     s70
     (COMMENT DeclaratorName "=>" "." DeclaratorName OP_DIM)
     (COMMENT DeclaratorName "=>" "." IDENTIFIER)
     (COMMENT MethodDeclarator "=>" "." MethodDeclarator OP_DIM)
     (COMMENT MethodDeclarator "=>" "." DeclaratorName LPAREN RPAREN)
     (COMMENT
      MethodDeclarator
      "=>"
      "."
      DeclaratorName
      LPAREN
      ParameterList
      RPAREN)
     (COMMENT
      MethodDeclaration
      "=>"
      TypeSpecifier
      "."
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      TypeSpecifier
      "."
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      VariableDeclarator
      "=>"
      "."
      DeclaratorName
      EQUAL_SIGN
      VariableInitializer)
     (COMMENT VariableDeclarator "=>" "." DeclaratorName)
     (COMMENT
      VariableDeclarators
      "=>"
      "."
      VariableDeclarators
      COMMA
      VariableDeclarator)
     (COMMENT VariableDeclarators "=>" "." VariableDeclarator)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      TypeSpecifier
      "."
      VariableDeclarators)
     (SHIFT (IDENTIFIER) s277)
     (GOTO VariableDeclarators s435)
     (GOTO VariableDeclarator s275)
     (GOTO MethodDeclarator s436)
     (GOTO DeclaratorName s423))
    (STATE
     s71
     (COMMENT Dims "=>" "." Dims OP_DIM)
     (COMMENT Dims "=>" "." OP_DIM)
     (COMMENT TypeSpecifier "=>" TypeName "." Dims)
     (COMMENT TypeSpecifier "=>" TypeName ".")
     (REDUCE (INSTANCEOF) r2)
     (REDUCE (OP_GE) r2)
     (REDUCE (OP_LE) r2)
     (REDUCE (OP_EQ) r2)
     (REDUCE (OP_NE) r2)
     (REDUCE (OP_LAND) r2)
     (REDUCE (OP_LOR) r2)
     (SHIFT (OP_DIM) s193)
     (REDUCE (IDENTIFIER) r2)
     (REDUCE (SEMICOLON) r2)
     (REDUCE (COMMA) r2)
     (REDUCE (RPAREN) r2)
     (REDUCE (RBRACK) r2)
     (REDUCE (RCURLY) r2)
     (REDUCE (LESS_THAN) r2)
     (REDUCE (GREATER_THAN) r2)
     (REDUCE (COLON) r2)
     (REDUCE (PIPE) r2)
     (REDUCE (QMARK) r2)
     (REDUCE (AMPERSAND) r2)
     (REDUCE (CARROT) r2)
     (GOTO Dims s434))
    (STATE
     s72
     (COMMENT FieldDeclaration "=>" TypeDeclaration ".")
     (REDUCE () r72))
    (STATE
     s73
     (COMMENT ConstructorDeclarator "=>" "." IDENTIFIER LPAREN RPAREN)
     (COMMENT
      ConstructorDeclarator
      "=>"
      "."
      IDENTIFIER
      LPAREN
      ParameterList
      RPAREN)
     (COMMENT
      ConstructorDeclaration
      "=>"
      Modifiers
      "."
      ConstructorDeclarator
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      Modifiers
      "."
      ConstructorDeclarator
      Throws
      Block)
     (COMMENT
      MethodDeclaration
      "=>"
      Modifiers
      "."
      TypeSpecifier
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      Modifiers
      "."
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      Modifiers
      "."
      TypeSpecifier
      VariableDeclarators)
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" Modifiers "." Modifier)
     (COMMENT ClassHeader "=>" Modifiers "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" Modifiers "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" Modifiers "." ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      Modifiers
      "."
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (CLASS) s14)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s15)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (INTERFACE) s17)
     (SHIFT (LONG) s57)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (SHORT) s58)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOID) s59)
     (SHIFT (VOLATILE) s26)
     (SHIFT (IDENTIFIER) s85)
     (GOTO TypeSpecifier s416)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50)
     (GOTO Modifier s63)
     (GOTO ClassWord s64)
     (GOTO ConstructorDeclarator s417))
    (STATE
     s74
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT NonStaticInitializer "=>" "." Block)
     (COMMENT StaticInitializer "=>" "." STATIC Block)
     (COMMENT ConstructorDeclarator "=>" "." IDENTIFIER LPAREN RPAREN)
     (COMMENT
      ConstructorDeclarator
      "=>"
      "."
      IDENTIFIER
      LPAREN
      ParameterList
      RPAREN)
     (COMMENT ConstructorDeclaration "=>" "." ConstructorDeclarator Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      ConstructorDeclarator
      Throws
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      Modifiers
      ConstructorDeclarator
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      Modifiers
      ConstructorDeclarator
      Throws
      Block)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      TypeSpecifier
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      "."
      TypeSpecifier
      VariableDeclarators)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      VariableDeclarators)
     (COMMENT FieldDeclaration "=>" "." TypeDeclaration)
     (COMMENT FieldDeclaration "=>" "." NonStaticInitializer)
     (COMMENT FieldDeclaration "=>" "." StaticInitializer)
     (COMMENT FieldDeclaration "=>" "." ConstructorDeclaration)
     (COMMENT FieldDeclaration "=>" "." MethodDeclaration)
     (COMMENT FieldDeclaration "=>" "." FieldVariableDeclaration SEMICOLON)
     (COMMENT FieldDeclarationOptSemi "=>" "." FieldDeclaration SemiColons)
     (COMMENT FieldDeclarationOptSemi "=>" "." FieldDeclaration)
     (COMMENT
      FieldDeclarations
      "=>"
      FieldDeclarations
      "."
      FieldDeclarationOptSemi)
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      ClassHeader
      LCURLY
      FieldDeclarations
      "."
      RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (CLASS) s14)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s15)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (INTERFACE) s17)
     (SHIFT (LONG) s57)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (SHORT) s58)
     (SHIFT (STATIC) s84)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOID) s59)
     (SHIFT (VOLATILE) s26)
     (SHIFT (IDENTIFIER) s85)
     (SHIFT (LCURLY) s86)
     (SHIFT (RCURLY) s415)
     (GOTO TypeSpecifier s70)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50)
     (GOTO TypeDeclaration s72)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s73)
     (GOTO Modifier s11)
     (GOTO ClassWord s12)
     (GOTO FieldDeclarationOptSemi s379)
     (GOTO FieldDeclaration s76)
     (GOTO FieldVariableDeclaration s77)
     (GOTO MethodDeclaration s78)
     (GOTO ConstructorDeclaration s79)
     (GOTO ConstructorDeclarator s80)
     (GOTO StaticInitializer s81)
     (GOTO NonStaticInitializer s82)
     (GOTO Block s83))
    (STATE
     s75
     (COMMENT FieldDeclarations "=>" FieldDeclarationOptSemi ".")
     (REDUCE () r63))
    (STATE
     s76
     (COMMENT FieldDeclarationOptSemi "=>" FieldDeclaration "." SemiColons)
     (COMMENT FieldDeclarationOptSemi "=>" FieldDeclaration ".")
     (COMMENT SemiColons "=>" "." SemiColons SEMICOLON)
     (COMMENT SemiColons "=>" "." SEMICOLON)
     (REDUCE (ABSTRACT) r65)
     (REDUCE (BOOLEAN) r65)
     (REDUCE (BYTE) r65)
     (REDUCE (CHAR) r65)
     (REDUCE (CLASS) r65)
     (REDUCE (DOUBLE) r65)
     (REDUCE (FINAL) r65)
     (REDUCE (FLOAT) r65)
     (REDUCE (INT) r65)
     (REDUCE (INTERFACE) r65)
     (REDUCE (LONG) r65)
     (REDUCE (NATIVE) r65)
     (REDUCE (PRIVATE) r65)
     (REDUCE (PROTECTED) r65)
     (REDUCE (PUBLIC) r65)
     (REDUCE (SHORT) r65)
     (REDUCE (STATIC) r65)
     (REDUCE (SYNCHRONIZED) r65)
     (REDUCE (TRANSIENT) r65)
     (REDUCE (VOID) r65)
     (REDUCE (VOLATILE) r65)
     (REDUCE (IDENTIFIER) r65)
     (SHIFT (SEMICOLON) s30)
     (REDUCE (LCURLY) r65)
     (REDUCE (RCURLY) r65)
     (GOTO SemiColons s414))
    (STATE
     s77
     (COMMENT FieldDeclaration "=>" FieldVariableDeclaration "." SEMICOLON)
     (SHIFT (SEMICOLON) s413))
    (STATE
     s78
     (COMMENT FieldDeclaration "=>" MethodDeclaration ".")
     (REDUCE () r68))
    (STATE
     s79
     (COMMENT FieldDeclaration "=>" ConstructorDeclaration ".")
     (REDUCE () r69))
    (STATE
     s80
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT ConstructorDeclaration "=>" ConstructorDeclarator "." Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      ConstructorDeclarator
      "."
      Throws
      Block)
     (COMMENT Throws "=>" "." THROWS ClassNameList)
     (SHIFT (THROWS) s410)
     (SHIFT (LCURLY) s86)
     (GOTO Throws s408)
     (GOTO Block s409))
    (STATE
     s81
     (COMMENT FieldDeclaration "=>" StaticInitializer ".")
     (REDUCE () r70))
    (STATE
     s82
     (COMMENT FieldDeclaration "=>" NonStaticInitializer ".")
     (REDUCE () r71))
    (STATE s83 (COMMENT NonStaticInitializer "=>" Block ".") (REDUCE () r108))
    (STATE
     s84
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT StaticInitializer "=>" STATIC "." Block)
     (COMMENT Modifier "=>" STATIC ".")
     (REDUCE (ABSTRACT) r55)
     (REDUCE (BOOLEAN) r55)
     (REDUCE (BYTE) r55)
     (REDUCE (CHAR) r55)
     (REDUCE (CLASS) r55)
     (REDUCE (DOUBLE) r55)
     (REDUCE (FINAL) r55)
     (REDUCE (FLOAT) r55)
     (REDUCE (INT) r55)
     (REDUCE (INTERFACE) r55)
     (REDUCE (LONG) r55)
     (REDUCE (NATIVE) r55)
     (REDUCE (PRIVATE) r55)
     (REDUCE (PROTECTED) r55)
     (REDUCE (PUBLIC) r55)
     (REDUCE (SHORT) r55)
     (REDUCE (STATIC) r55)
     (REDUCE (SYNCHRONIZED) r55)
     (REDUCE (TRANSIENT) r55)
     (REDUCE (VOID) r55)
     (REDUCE (VOLATILE) r55)
     (REDUCE (IDENTIFIER) r55)
     (SHIFT (LCURLY) s86)
     (GOTO Block s407))
    (STATE
     s85
     (COMMENT ConstructorDeclarator "=>" IDENTIFIER "." LPAREN RPAREN)
     (COMMENT
      ConstructorDeclarator
      "=>"
      IDENTIFIER
      "."
      LPAREN
      ParameterList
      RPAREN)
     (COMMENT QualifiedName "=>" IDENTIFIER ".")
     (REDUCE (OP_DIM) r36)
     (REDUCE (IDENTIFIER) r36)
     (SHIFT (LPAREN) s395)
     (REDUCE (DOT) r36))
    (STATE
     s86
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches Finally)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches)
     (COMMENT GuardingStatement "=>" "." TRY Block Finally)
     (COMMENT
      GuardingStatement
      "=>"
      "."
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT JumpStatement "=>" "." THROW Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE IDENTIFIER SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK IDENTIFIER SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT LabelStatement "=>" "." DEFAULT COLON)
     (COMMENT LabelStatement "=>" "." CASE ConstantExpression COLON)
     (COMMENT LabelStatement "=>" "." IDENTIFIER COLON)
     (COMMENT EmptyStatement "=>" "." SEMICOLON)
     (COMMENT Statement "=>" "." Block)
     (COMMENT Statement "=>" "." GuardingStatement)
     (COMMENT Statement "=>" "." JumpStatement)
     (COMMENT Statement "=>" "." IterationStatement)
     (COMMENT Statement "=>" "." SelectionStatement)
     (COMMENT Statement "=>" "." ExpressionStatement SEMICOLON)
     (COMMENT Statement "=>" "." LabelStatement)
     (COMMENT Statement "=>" "." EmptyStatement)
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      "."
      FINAL
      TypeSpecifier
      VariableDeclarators
      SEMICOLON)
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      "."
      TypeSpecifier
      VariableDeclarators
      SEMICOLON)
     (COMMENT LocalVariableDeclarationOrStatement "=>" "." Statement)
     (COMMENT
      LocalVariableDeclarationOrStatement
      "=>"
      "."
      LocalVariableDeclarationStatement)
     (COMMENT
      LocalVariableDeclarationsAndStatements
      "=>"
      "."
      LocalVariableDeclarationsAndStatements
      LocalVariableDeclarationOrStatement)
     (COMMENT
      LocalVariableDeclarationsAndStatements
      "=>"
      "."
      LocalVariableDeclarationOrStatement)
     (COMMENT Block "=>" LCURLY "." RCURLY)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      LCURLY
      "."
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BREAK) s136)
     (SHIFT (BYTE) s52)
     (SHIFT (CASE) s137)
     (SHIFT (CHAR) s53)
     (SHIFT (CONTINUE) s138)
     (SHIFT (DEFAULT) s139)
     (SHIFT (DO) s140)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s141)
     (SHIFT (FLOAT) s55)
     (SHIFT (FOR) s142)
     (SHIFT (IF) s143)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (RETURN) s146)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (SWITCH) s148)
     (SHIFT (SYNCHRONIZED) s149)
     (SHIFT (THIS) s150)
     (SHIFT (THROW) s151)
     (SHIFT (TRY) s152)
     (SHIFT (VOID) s59)
     (SHIFT (WHILE) s153)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s156)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s159)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s86)
     (SHIFT (RCURLY) s161)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO TypeSpecifier s88)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s89)
     (GOTO QualifiedName s90)
     (GOTO Block s91)
     (GOTO LocalVariableDeclarationsAndStatements s92)
     (GOTO LocalVariableDeclarationOrStatement s93)
     (GOTO LocalVariableDeclarationStatement s94)
     (GOTO Statement s95)
     (GOTO EmptyStatement s96)
     (GOTO LabelStatement s97)
     (GOTO ExpressionStatement s98)
     (GOTO SelectionStatement s99)
     (GOTO IterationStatement s100)
     (GOTO JumpStatement s101)
     (GOTO GuardingStatement s102)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s87
     (COMMENT TypeDeclaration "=>" ClassHeader LCURLY RCURLY ".")
     (REDUCE () r39))
    (STATE
     s88
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      TypeSpecifier
      "."
      VariableDeclarators
      SEMICOLON)
     (COMMENT DeclaratorName "=>" "." DeclaratorName OP_DIM)
     (COMMENT DeclaratorName "=>" "." IDENTIFIER)
     (COMMENT
      VariableDeclarator
      "=>"
      "."
      DeclaratorName
      EQUAL_SIGN
      VariableInitializer)
     (COMMENT VariableDeclarator "=>" "." DeclaratorName)
     (COMMENT
      VariableDeclarators
      "=>"
      "."
      VariableDeclarators
      COMMA
      VariableDeclarator)
     (COMMENT VariableDeclarators "=>" "." VariableDeclarator)
     (SHIFT (IDENTIFIER) s277)
     (GOTO VariableDeclarators s393)
     (GOTO VariableDeclarator s275)
     (GOTO DeclaratorName s276))
    (STATE
     s89
     (COMMENT FieldAccess "=>" PrimitiveType "." DOT CLASS)
     (COMMENT TypeName "=>" PrimitiveType ".")
     (REDUCE (OP_DIM) r4)
     (REDUCE (IDENTIFIER) r4)
     (SHIFT (DOT) s185))
    (STATE
     s90
     (COMMENT
      NewAllocationExpression
      "=>"
      QualifiedName
      "."
      DOT
      PlainNewAllocationExpression)
     (COMMENT MethodAccess "=>" QualifiedName ".")
     (COMMENT FieldAccess "=>" QualifiedName "." DOT CLASS)
     (COMMENT FieldAccess "=>" QualifiedName "." DOT THIS)
     (COMMENT ArrayAccess "=>" QualifiedName "." LBRACK Expression RBRACK)
     (COMMENT PrimaryExpression "=>" QualifiedName ".")
     (COMMENT QualifiedName "=>" QualifiedName "." DOT IDENTIFIER)
     (COMMENT TypeName "=>" QualifiedName ".")
     (REDUCE (INSTANCEOF) r164)
     (REDUCE (OP_INC) r164)
     (REDUCE (OP_DEC) r164)
     (REDUCE (OP_SHL) r164)
     (REDUCE (OP_SHR) r164)
     (REDUCE (OP_SHRR) r164)
     (REDUCE (OP_GE) r164)
     (REDUCE (OP_LE) r164)
     (REDUCE (OP_EQ) r164)
     (REDUCE (OP_NE) r164)
     (REDUCE (OP_LAND) r164)
     (REDUCE (OP_LOR) r164)
     (REDUCE (OP_DIM) r5)
     (REDUCE (ASS_MUL) r164)
     (REDUCE (ASS_DIV) r164)
     (REDUCE (ASS_MOD) r164)
     (REDUCE (ASS_ADD) r164)
     (REDUCE (ASS_SUB) r164)
     (REDUCE (ASS_SHL) r164)
     (REDUCE (ASS_SHR) r164)
     (REDUCE (ASS_SHRR) r164)
     (REDUCE (ASS_AND) r164)
     (REDUCE (ASS_XOR) r164)
     (REDUCE (ASS_OR) r164)
     (REDUCE (IDENTIFIER) r5)
     (REDUCE (SEMICOLON) r164)
     (REDUCE (COMMA) r164)
     (REDUCE (LPAREN) r187)
     (SHIFT (LBRACK) s178)
     (REDUCE (EQUAL_SIGN) r164)
     (SHIFT (DOT) s179)
     (REDUCE (PLUS) r164)
     (REDUCE (STAR) r164)
     (REDUCE (FORWARD_SLASH) r164)
     (REDUCE (MINUS) r164)
     (REDUCE (PERCENT) r164)
     (REDUCE (LESS_THAN) r164)
     (REDUCE (GREATER_THAN) r164)
     (REDUCE (PIPE) r164)
     (REDUCE (QMARK) r164)
     (REDUCE (AMPERSAND) r164)
     (REDUCE (CARROT) r164))
    (STATE s91 (COMMENT Statement "=>" Block ".") (REDUCE () r126))
    (STATE
     s92
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches Finally)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches)
     (COMMENT GuardingStatement "=>" "." TRY Block Finally)
     (COMMENT
      GuardingStatement
      "=>"
      "."
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT JumpStatement "=>" "." THROW Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE IDENTIFIER SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK IDENTIFIER SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT LabelStatement "=>" "." DEFAULT COLON)
     (COMMENT LabelStatement "=>" "." CASE ConstantExpression COLON)
     (COMMENT LabelStatement "=>" "." IDENTIFIER COLON)
     (COMMENT EmptyStatement "=>" "." SEMICOLON)
     (COMMENT Statement "=>" "." Block)
     (COMMENT Statement "=>" "." GuardingStatement)
     (COMMENT Statement "=>" "." JumpStatement)
     (COMMENT Statement "=>" "." IterationStatement)
     (COMMENT Statement "=>" "." SelectionStatement)
     (COMMENT Statement "=>" "." ExpressionStatement SEMICOLON)
     (COMMENT Statement "=>" "." LabelStatement)
     (COMMENT Statement "=>" "." EmptyStatement)
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      "."
      FINAL
      TypeSpecifier
      VariableDeclarators
      SEMICOLON)
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      "."
      TypeSpecifier
      VariableDeclarators
      SEMICOLON)
     (COMMENT LocalVariableDeclarationOrStatement "=>" "." Statement)
     (COMMENT
      LocalVariableDeclarationOrStatement
      "=>"
      "."
      LocalVariableDeclarationStatement)
     (COMMENT
      LocalVariableDeclarationsAndStatements
      "=>"
      LocalVariableDeclarationsAndStatements
      "."
      LocalVariableDeclarationOrStatement)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      LCURLY
      LocalVariableDeclarationsAndStatements
      "."
      RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BREAK) s136)
     (SHIFT (BYTE) s52)
     (SHIFT (CASE) s137)
     (SHIFT (CHAR) s53)
     (SHIFT (CONTINUE) s138)
     (SHIFT (DEFAULT) s139)
     (SHIFT (DO) s140)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s141)
     (SHIFT (FLOAT) s55)
     (SHIFT (FOR) s142)
     (SHIFT (IF) s143)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (RETURN) s146)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (SWITCH) s148)
     (SHIFT (SYNCHRONIZED) s149)
     (SHIFT (THIS) s150)
     (SHIFT (THROW) s151)
     (SHIFT (TRY) s152)
     (SHIFT (VOID) s59)
     (SHIFT (WHILE) s153)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s156)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s159)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s86)
     (SHIFT (RCURLY) s392)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO TypeSpecifier s88)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s89)
     (GOTO QualifiedName s90)
     (GOTO Block s91)
     (GOTO LocalVariableDeclarationOrStatement s391)
     (GOTO LocalVariableDeclarationStatement s94)
     (GOTO Statement s95)
     (GOTO EmptyStatement s96)
     (GOTO LabelStatement s97)
     (GOTO ExpressionStatement s98)
     (GOTO SelectionStatement s99)
     (GOTO IterationStatement s100)
     (GOTO JumpStatement s101)
     (GOTO GuardingStatement s102)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s93
     (COMMENT
      LocalVariableDeclarationsAndStatements
      "=>"
      LocalVariableDeclarationOrStatement
      ".")
     (REDUCE () r113))
    (STATE
     s94
     (COMMENT
      LocalVariableDeclarationOrStatement
      "=>"
      LocalVariableDeclarationStatement
      ".")
     (REDUCE () r115))
    (STATE
     s95
     (COMMENT LocalVariableDeclarationOrStatement "=>" Statement ".")
     (REDUCE () r116))
    (STATE s96 (COMMENT Statement "=>" EmptyStatement ".") (REDUCE () r119))
    (STATE s97 (COMMENT Statement "=>" LabelStatement ".") (REDUCE () r120))
    (STATE
     s98
     (COMMENT Statement "=>" ExpressionStatement "." SEMICOLON)
     (SHIFT (SEMICOLON) s390))
    (STATE
     s99
     (COMMENT Statement "=>" SelectionStatement ".")
     (REDUCE () r122))
    (STATE
     s100
     (COMMENT Statement "=>" IterationStatement ".")
     (REDUCE () r123))
    (STATE s101 (COMMENT Statement "=>" JumpStatement ".") (REDUCE () r124))
    (STATE
     s102
     (COMMENT Statement "=>" GuardingStatement ".")
     (REDUCE () r125))
    (STATE
     s103
     (COMMENT PostfixExpression "=>" PrimaryExpression ".")
     (REDUCE () r211))
    (STATE
     s104
     (COMMENT FieldAccess "=>" NotJustName "." DOT IDENTIFIER)
     (COMMENT PrimaryExpression "=>" NotJustName ".")
     (REDUCE (INSTANCEOF) r165)
     (REDUCE (OP_INC) r165)
     (REDUCE (OP_DEC) r165)
     (REDUCE (OP_SHL) r165)
     (REDUCE (OP_SHR) r165)
     (REDUCE (OP_SHRR) r165)
     (REDUCE (OP_GE) r165)
     (REDUCE (OP_LE) r165)
     (REDUCE (OP_EQ) r165)
     (REDUCE (OP_NE) r165)
     (REDUCE (OP_LAND) r165)
     (REDUCE (OP_LOR) r165)
     (REDUCE (ASS_MUL) r165)
     (REDUCE (ASS_DIV) r165)
     (REDUCE (ASS_MOD) r165)
     (REDUCE (ASS_ADD) r165)
     (REDUCE (ASS_SUB) r165)
     (REDUCE (ASS_SHL) r165)
     (REDUCE (ASS_SHR) r165)
     (REDUCE (ASS_SHRR) r165)
     (REDUCE (ASS_AND) r165)
     (REDUCE (ASS_XOR) r165)
     (REDUCE (ASS_OR) r165)
     (REDUCE (SEMICOLON) r165)
     (REDUCE (COMMA) r165)
     (REDUCE (RPAREN) r165)
     (REDUCE (RBRACK) r165)
     (REDUCE (RCURLY) r165)
     (REDUCE (EQUAL_SIGN) r165)
     (SHIFT (DOT) s388)
     (REDUCE (PLUS) r165)
     (REDUCE (STAR) r165)
     (REDUCE (FORWARD_SLASH) r165)
     (REDUCE (MINUS) r165)
     (REDUCE (PERCENT) r165)
     (REDUCE (LESS_THAN) r165)
     (REDUCE (GREATER_THAN) r165)
     (REDUCE (COLON) r165)
     (REDUCE (PIPE) r165)
     (REDUCE (QMARK) r165)
     (REDUCE (AMPERSAND) r165)
     (REDUCE (CARROT) r165))
    (STATE
     s105
     (COMMENT ArrayAccess "=>" ComplexPrimary "." LBRACK Expression RBRACK)
     (COMMENT NotJustName "=>" ComplexPrimary ".")
     (REDUCE (INSTANCEOF) r168)
     (REDUCE (OP_INC) r168)
     (REDUCE (OP_DEC) r168)
     (REDUCE (OP_SHL) r168)
     (REDUCE (OP_SHR) r168)
     (REDUCE (OP_SHRR) r168)
     (REDUCE (OP_GE) r168)
     (REDUCE (OP_LE) r168)
     (REDUCE (OP_EQ) r168)
     (REDUCE (OP_NE) r168)
     (REDUCE (OP_LAND) r168)
     (REDUCE (OP_LOR) r168)
     (REDUCE (ASS_MUL) r168)
     (REDUCE (ASS_DIV) r168)
     (REDUCE (ASS_MOD) r168)
     (REDUCE (ASS_ADD) r168)
     (REDUCE (ASS_SUB) r168)
     (REDUCE (ASS_SHL) r168)
     (REDUCE (ASS_SHR) r168)
     (REDUCE (ASS_SHRR) r168)
     (REDUCE (ASS_AND) r168)
     (REDUCE (ASS_XOR) r168)
     (REDUCE (ASS_OR) r168)
     (REDUCE (SEMICOLON) r168)
     (REDUCE (COMMA) r168)
     (REDUCE (RPAREN) r168)
     (SHIFT (LBRACK) s385)
     (REDUCE (RBRACK) r168)
     (REDUCE (RCURLY) r168)
     (REDUCE (EQUAL_SIGN) r168)
     (REDUCE (DOT) r168)
     (REDUCE (PLUS) r168)
     (REDUCE (STAR) r168)
     (REDUCE (FORWARD_SLASH) r168)
     (REDUCE (MINUS) r168)
     (REDUCE (PERCENT) r168)
     (REDUCE (LESS_THAN) r168)
     (REDUCE (GREATER_THAN) r168)
     (REDUCE (COLON) r168)
     (REDUCE (PIPE) r168)
     (REDUCE (QMARK) r168)
     (REDUCE (AMPERSAND) r168)
     (REDUCE (CARROT) r168))
    (STATE
     s106
     (COMMENT MethodAccess "=>" ComplexPrimaryNoParenthesis ".")
     (COMMENT ComplexPrimary "=>" ComplexPrimaryNoParenthesis ".")
     (REDUCE (INSTANCEOF) r170)
     (REDUCE (OP_INC) r170)
     (REDUCE (OP_DEC) r170)
     (REDUCE (OP_SHL) r170)
     (REDUCE (OP_SHR) r170)
     (REDUCE (OP_SHRR) r170)
     (REDUCE (OP_GE) r170)
     (REDUCE (OP_LE) r170)
     (REDUCE (OP_EQ) r170)
     (REDUCE (OP_NE) r170)
     (REDUCE (OP_LAND) r170)
     (REDUCE (OP_LOR) r170)
     (REDUCE (ASS_MUL) r170)
     (REDUCE (ASS_DIV) r170)
     (REDUCE (ASS_MOD) r170)
     (REDUCE (ASS_ADD) r170)
     (REDUCE (ASS_SUB) r170)
     (REDUCE (ASS_SHL) r170)
     (REDUCE (ASS_SHR) r170)
     (REDUCE (ASS_SHRR) r170)
     (REDUCE (ASS_AND) r170)
     (REDUCE (ASS_XOR) r170)
     (REDUCE (ASS_OR) r170)
     (REDUCE (SEMICOLON) r170)
     (REDUCE (COMMA) r170)
     (REDUCE (LPAREN) r185)
     (REDUCE (RPAREN) r170)
     (REDUCE (LBRACK) r170)
     (REDUCE (RBRACK) r170)
     (REDUCE (RCURLY) r170)
     (REDUCE (EQUAL_SIGN) r170)
     (REDUCE (DOT) r170)
     (REDUCE (PLUS) r170)
     (REDUCE (STAR) r170)
     (REDUCE (FORWARD_SLASH) r170)
     (REDUCE (MINUS) r170)
     (REDUCE (PERCENT) r170)
     (REDUCE (LESS_THAN) r170)
     (REDUCE (GREATER_THAN) r170)
     (REDUCE (COLON) r170)
     (REDUCE (PIPE) r170)
     (REDUCE (QMARK) r170)
     (REDUCE (AMPERSAND) r170)
     (REDUCE (CARROT) r170))
    (STATE
     s107
     (COMMENT ComplexPrimaryNoParenthesis "=>" ArrayAccess ".")
     (REDUCE () r173))
    (STATE
     s108
     (COMMENT ComplexPrimaryNoParenthesis "=>" FieldAccess ".")
     (REDUCE () r174))
    (STATE
     s109
     (COMMENT ComplexPrimaryNoParenthesis "=>" MethodCall ".")
     (REDUCE () r175))
    (STATE
     s110
     (COMMENT MethodCall "=>" MethodAccess "." LPAREN RPAREN)
     (COMMENT MethodCall "=>" MethodAccess "." LPAREN ArgumentList RPAREN)
     (SHIFT (LPAREN) s381))
    (STATE
     s111
     (COMMENT MethodAccess "=>" SpecialName ".")
     (COMMENT NotJustName "=>" SpecialName ".")
     (REDUCE (INSTANCEOF) r166)
     (REDUCE (OP_INC) r166)
     (REDUCE (OP_DEC) r166)
     (REDUCE (OP_SHL) r166)
     (REDUCE (OP_SHR) r166)
     (REDUCE (OP_SHRR) r166)
     (REDUCE (OP_GE) r166)
     (REDUCE (OP_LE) r166)
     (REDUCE (OP_EQ) r166)
     (REDUCE (OP_NE) r166)
     (REDUCE (OP_LAND) r166)
     (REDUCE (OP_LOR) r166)
     (REDUCE (ASS_MUL) r166)
     (REDUCE (ASS_DIV) r166)
     (REDUCE (ASS_MOD) r166)
     (REDUCE (ASS_ADD) r166)
     (REDUCE (ASS_SUB) r166)
     (REDUCE (ASS_SHL) r166)
     (REDUCE (ASS_SHR) r166)
     (REDUCE (ASS_SHRR) r166)
     (REDUCE (ASS_AND) r166)
     (REDUCE (ASS_XOR) r166)
     (REDUCE (ASS_OR) r166)
     (REDUCE (SEMICOLON) r166)
     (REDUCE (COMMA) r166)
     (REDUCE (LPAREN) r186)
     (REDUCE (RPAREN) r166)
     (REDUCE (RBRACK) r166)
     (REDUCE (RCURLY) r166)
     (REDUCE (EQUAL_SIGN) r166)
     (REDUCE (DOT) r166)
     (REDUCE (PLUS) r166)
     (REDUCE (STAR) r166)
     (REDUCE (FORWARD_SLASH) r166)
     (REDUCE (MINUS) r166)
     (REDUCE (PERCENT) r166)
     (REDUCE (LESS_THAN) r166)
     (REDUCE (GREATER_THAN) r166)
     (REDUCE (COLON) r166)
     (REDUCE (PIPE) r166)
     (REDUCE (QMARK) r166)
     (REDUCE (AMPERSAND) r166)
     (REDUCE (CARROT) r166))
    (STATE
     s112
     (COMMENT NotJustName "=>" NewAllocationExpression ".")
     (REDUCE () r167))
    (STATE
     s113
     (COMMENT NewAllocationExpression "=>" PlainNewAllocationExpression ".")
     (REDUCE () r193))
    (STATE
     s114
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ClassAllocationExpression
      "."
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ClassAllocationExpression
      "."
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" ClassAllocationExpression ".")
     (REDUCE (INSTANCEOF) r196)
     (REDUCE (OP_INC) r196)
     (REDUCE (OP_DEC) r196)
     (REDUCE (OP_SHL) r196)
     (REDUCE (OP_SHR) r196)
     (REDUCE (OP_SHRR) r196)
     (REDUCE (OP_GE) r196)
     (REDUCE (OP_LE) r196)
     (REDUCE (OP_EQ) r196)
     (REDUCE (OP_NE) r196)
     (REDUCE (OP_LAND) r196)
     (REDUCE (OP_LOR) r196)
     (REDUCE (ASS_MUL) r196)
     (REDUCE (ASS_DIV) r196)
     (REDUCE (ASS_MOD) r196)
     (REDUCE (ASS_ADD) r196)
     (REDUCE (ASS_SUB) r196)
     (REDUCE (ASS_SHL) r196)
     (REDUCE (ASS_SHR) r196)
     (REDUCE (ASS_SHRR) r196)
     (REDUCE (ASS_AND) r196)
     (REDUCE (ASS_XOR) r196)
     (REDUCE (ASS_OR) r196)
     (REDUCE (SEMICOLON) r196)
     (REDUCE (COMMA) r196)
     (REDUCE (RPAREN) r196)
     (REDUCE (RBRACK) r196)
     (SHIFT (LCURLY) s376)
     (REDUCE (RCURLY) r196)
     (REDUCE (EQUAL_SIGN) r196)
     (REDUCE (DOT) r196)
     (REDUCE (PLUS) r196)
     (REDUCE (STAR) r196)
     (REDUCE (FORWARD_SLASH) r196)
     (REDUCE (MINUS) r196)
     (REDUCE (PERCENT) r196)
     (REDUCE (LESS_THAN) r196)
     (REDUCE (GREATER_THAN) r196)
     (REDUCE (COLON) r196)
     (REDUCE (PIPE) r196)
     (REDUCE (QMARK) r196)
     (REDUCE (AMPERSAND) r196)
     (REDUCE (CARROT) r196))
    (STATE
     s115
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ArrayAllocationExpression
      "."
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ArrayAllocationExpression
      "."
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" ArrayAllocationExpression ".")
     (REDUCE (INSTANCEOF) r195)
     (REDUCE (OP_INC) r195)
     (REDUCE (OP_DEC) r195)
     (REDUCE (OP_SHL) r195)
     (REDUCE (OP_SHR) r195)
     (REDUCE (OP_SHRR) r195)
     (REDUCE (OP_GE) r195)
     (REDUCE (OP_LE) r195)
     (REDUCE (OP_EQ) r195)
     (REDUCE (OP_NE) r195)
     (REDUCE (OP_LAND) r195)
     (REDUCE (OP_LOR) r195)
     (REDUCE (ASS_MUL) r195)
     (REDUCE (ASS_DIV) r195)
     (REDUCE (ASS_MOD) r195)
     (REDUCE (ASS_ADD) r195)
     (REDUCE (ASS_SUB) r195)
     (REDUCE (ASS_SHL) r195)
     (REDUCE (ASS_SHR) r195)
     (REDUCE (ASS_SHRR) r195)
     (REDUCE (ASS_AND) r195)
     (REDUCE (ASS_XOR) r195)
     (REDUCE (ASS_OR) r195)
     (REDUCE (SEMICOLON) r195)
     (REDUCE (COMMA) r195)
     (REDUCE (RPAREN) r195)
     (REDUCE (RBRACK) r195)
     (SHIFT (LCURLY) s372)
     (REDUCE (RCURLY) r195)
     (REDUCE (EQUAL_SIGN) r195)
     (REDUCE (DOT) r195)
     (REDUCE (PLUS) r195)
     (REDUCE (STAR) r195)
     (REDUCE (FORWARD_SLASH) r195)
     (REDUCE (MINUS) r195)
     (REDUCE (PERCENT) r195)
     (REDUCE (LESS_THAN) r195)
     (REDUCE (GREATER_THAN) r195)
     (REDUCE (COLON) r195)
     (REDUCE (PIPE) r195)
     (REDUCE (QMARK) r195)
     (REDUCE (AMPERSAND) r195)
     (REDUCE (CARROT) r195))
    (STATE
     s116
     (COMMENT LogicalUnaryExpression "=>" PostfixExpression ".")
     (COMMENT RealPostfixExpression "=>" PostfixExpression "." OP_DEC)
     (COMMENT RealPostfixExpression "=>" PostfixExpression "." OP_INC)
     (REDUCE (INSTANCEOF) r219)
     (SHIFT (OP_INC) s370)
     (SHIFT (OP_DEC) s371)
     (REDUCE (OP_SHL) r219)
     (REDUCE (OP_SHR) r219)
     (REDUCE (OP_SHRR) r219)
     (REDUCE (OP_GE) r219)
     (REDUCE (OP_LE) r219)
     (REDUCE (OP_EQ) r219)
     (REDUCE (OP_NE) r219)
     (REDUCE (OP_LAND) r219)
     (REDUCE (OP_LOR) r219)
     (REDUCE (ASS_MUL) r219)
     (REDUCE (ASS_DIV) r219)
     (REDUCE (ASS_MOD) r219)
     (REDUCE (ASS_ADD) r219)
     (REDUCE (ASS_SUB) r219)
     (REDUCE (ASS_SHL) r219)
     (REDUCE (ASS_SHR) r219)
     (REDUCE (ASS_SHRR) r219)
     (REDUCE (ASS_AND) r219)
     (REDUCE (ASS_XOR) r219)
     (REDUCE (ASS_OR) r219)
     (REDUCE (SEMICOLON) r219)
     (REDUCE (COMMA) r219)
     (REDUCE (RPAREN) r219)
     (REDUCE (RBRACK) r219)
     (REDUCE (RCURLY) r219)
     (REDUCE (EQUAL_SIGN) r219)
     (REDUCE (PLUS) r219)
     (REDUCE (STAR) r219)
     (REDUCE (FORWARD_SLASH) r219)
     (REDUCE (MINUS) r219)
     (REDUCE (PERCENT) r219)
     (REDUCE (LESS_THAN) r219)
     (REDUCE (GREATER_THAN) r219)
     (REDUCE (COLON) r219)
     (REDUCE (PIPE) r219)
     (REDUCE (QMARK) r219)
     (REDUCE (AMPERSAND) r219)
     (REDUCE (CARROT) r219))
    (STATE
     s117
     (COMMENT PostfixExpression "=>" RealPostfixExpression ".")
     (COMMENT FieldAccess "=>" RealPostfixExpression "." DOT IDENTIFIER)
     (REDUCE (INSTANCEOF) r212)
     (REDUCE (OP_INC) r212)
     (REDUCE (OP_DEC) r212)
     (REDUCE (OP_SHL) r212)
     (REDUCE (OP_SHR) r212)
     (REDUCE (OP_SHRR) r212)
     (REDUCE (OP_GE) r212)
     (REDUCE (OP_LE) r212)
     (REDUCE (OP_EQ) r212)
     (REDUCE (OP_NE) r212)
     (REDUCE (OP_LAND) r212)
     (REDUCE (OP_LOR) r212)
     (REDUCE (ASS_MUL) r212)
     (REDUCE (ASS_DIV) r212)
     (REDUCE (ASS_MOD) r212)
     (REDUCE (ASS_ADD) r212)
     (REDUCE (ASS_SUB) r212)
     (REDUCE (ASS_SHL) r212)
     (REDUCE (ASS_SHR) r212)
     (REDUCE (ASS_SHRR) r212)
     (REDUCE (ASS_AND) r212)
     (REDUCE (ASS_XOR) r212)
     (REDUCE (ASS_OR) r212)
     (REDUCE (SEMICOLON) r212)
     (REDUCE (COMMA) r212)
     (REDUCE (RPAREN) r212)
     (REDUCE (RBRACK) r212)
     (REDUCE (RCURLY) r212)
     (REDUCE (EQUAL_SIGN) r212)
     (SHIFT (DOT) s368)
     (REDUCE (PLUS) r212)
     (REDUCE (STAR) r212)
     (REDUCE (FORWARD_SLASH) r212)
     (REDUCE (MINUS) r212)
     (REDUCE (PERCENT) r212)
     (REDUCE (LESS_THAN) r212)
     (REDUCE (GREATER_THAN) r212)
     (REDUCE (COLON) r212)
     (REDUCE (PIPE) r212)
     (REDUCE (QMARK) r212)
     (REDUCE (AMPERSAND) r212)
     (REDUCE (CARROT) r212))
    (STATE
     s118
     (COMMENT AssignmentOperator "=>" "." ASS_OR)
     (COMMENT AssignmentOperator "=>" "." ASS_XOR)
     (COMMENT AssignmentOperator "=>" "." ASS_AND)
     (COMMENT AssignmentOperator "=>" "." ASS_SHRR)
     (COMMENT AssignmentOperator "=>" "." ASS_SHR)
     (COMMENT AssignmentOperator "=>" "." ASS_SHL)
     (COMMENT AssignmentOperator "=>" "." ASS_SUB)
     (COMMENT AssignmentOperator "=>" "." ASS_ADD)
     (COMMENT AssignmentOperator "=>" "." ASS_MOD)
     (COMMENT AssignmentOperator "=>" "." ASS_DIV)
     (COMMENT AssignmentOperator "=>" "." ASS_MUL)
     (COMMENT AssignmentOperator "=>" "." EQUAL_SIGN)
     (COMMENT
      AssignmentExpression
      "=>"
      UnaryExpression
      "."
      AssignmentOperator
      AssignmentExpression)
     (COMMENT CastExpression "=>" UnaryExpression ".")
     (REDUCE (INSTANCEOF) r225)
     (REDUCE (OP_SHL) r225)
     (REDUCE (OP_SHR) r225)
     (REDUCE (OP_SHRR) r225)
     (REDUCE (OP_GE) r225)
     (REDUCE (OP_LE) r225)
     (REDUCE (OP_EQ) r225)
     (REDUCE (OP_NE) r225)
     (REDUCE (OP_LAND) r225)
     (REDUCE (OP_LOR) r225)
     (SHIFT (ASS_MUL) s355)
     (SHIFT (ASS_DIV) s356)
     (SHIFT (ASS_MOD) s357)
     (SHIFT (ASS_ADD) s358)
     (SHIFT (ASS_SUB) s359)
     (SHIFT (ASS_SHL) s360)
     (SHIFT (ASS_SHR) s361)
     (SHIFT (ASS_SHRR) s362)
     (SHIFT (ASS_AND) s363)
     (SHIFT (ASS_XOR) s364)
     (SHIFT (ASS_OR) s365)
     (REDUCE (SEMICOLON) r225)
     (REDUCE (COMMA) r225)
     (REDUCE (RPAREN) r225)
     (REDUCE (RBRACK) r225)
     (REDUCE (RCURLY) r225)
     (SHIFT (EQUAL_SIGN) s366)
     (REDUCE (PLUS) r225)
     (REDUCE (STAR) r225)
     (REDUCE (FORWARD_SLASH) r225)
     (REDUCE (MINUS) r225)
     (REDUCE (PERCENT) r225)
     (REDUCE (LESS_THAN) r225)
     (REDUCE (GREATER_THAN) r225)
     (REDUCE (COLON) r225)
     (REDUCE (PIPE) r225)
     (REDUCE (QMARK) r225)
     (REDUCE (AMPERSAND) r225)
     (REDUCE (CARROT) r225)
     (GOTO AssignmentOperator s354))
    (STATE
     s119
     (COMMENT UnaryExpression "=>" LogicalUnaryExpression ".")
     (REDUCE () r218))
    (STATE
     s120
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      LogicalUnaryOperator
      "."
      UnaryExpression)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s175)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s353)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121))
    (STATE
     s121
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" ArithmeticUnaryOperator "." CastExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s352))
    (STATE
     s122
     (COMMENT MultiplicativeExpression "=>" CastExpression ".")
     (REDUCE () r232))
    (STATE
     s123
     (COMMENT AdditiveExpression "=>" MultiplicativeExpression ".")
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      "."
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      "."
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      "."
      STAR
      CastExpression)
     (REDUCE (INSTANCEOF) r236)
     (REDUCE (OP_SHL) r236)
     (REDUCE (OP_SHR) r236)
     (REDUCE (OP_SHRR) r236)
     (REDUCE (OP_GE) r236)
     (REDUCE (OP_LE) r236)
     (REDUCE (OP_EQ) r236)
     (REDUCE (OP_NE) r236)
     (REDUCE (OP_LAND) r236)
     (REDUCE (OP_LOR) r236)
     (REDUCE (SEMICOLON) r236)
     (REDUCE (COMMA) r236)
     (REDUCE (RPAREN) r236)
     (REDUCE (RBRACK) r236)
     (REDUCE (RCURLY) r236)
     (REDUCE (PLUS) r236)
     (SHIFT (STAR) s338)
     (SHIFT (FORWARD_SLASH) s339)
     (REDUCE (MINUS) r236)
     (SHIFT (PERCENT) s340)
     (REDUCE (LESS_THAN) r236)
     (REDUCE (GREATER_THAN) r236)
     (REDUCE (COLON) r236)
     (REDUCE (PIPE) r236)
     (REDUCE (QMARK) r236)
     (REDUCE (AMPERSAND) r236)
     (REDUCE (CARROT) r236))
    (STATE
     s124
     (COMMENT ShiftExpression "=>" AdditiveExpression ".")
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      "."
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      "."
      PLUS
      MultiplicativeExpression)
     (REDUCE (INSTANCEOF) r239)
     (REDUCE (OP_SHL) r239)
     (REDUCE (OP_SHR) r239)
     (REDUCE (OP_SHRR) r239)
     (REDUCE (OP_GE) r239)
     (REDUCE (OP_LE) r239)
     (REDUCE (OP_EQ) r239)
     (REDUCE (OP_NE) r239)
     (REDUCE (OP_LAND) r239)
     (REDUCE (OP_LOR) r239)
     (REDUCE (SEMICOLON) r239)
     (REDUCE (COMMA) r239)
     (REDUCE (RPAREN) r239)
     (REDUCE (RBRACK) r239)
     (REDUCE (RCURLY) r239)
     (SHIFT (PLUS) s335)
     (SHIFT (MINUS) s336)
     (REDUCE (LESS_THAN) r239)
     (REDUCE (GREATER_THAN) r239)
     (REDUCE (COLON) r239)
     (REDUCE (PIPE) r239)
     (REDUCE (QMARK) r239)
     (REDUCE (AMPERSAND) r239)
     (REDUCE (CARROT) r239))
    (STATE
     s125
     (COMMENT RelationalExpression "=>" ShiftExpression ".")
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHL
      AdditiveExpression)
     (REDUCE (INSTANCEOF) r243)
     (SHIFT (OP_SHL) s331)
     (SHIFT (OP_SHR) s332)
     (SHIFT (OP_SHRR) s333)
     (REDUCE (OP_GE) r243)
     (REDUCE (OP_LE) r243)
     (REDUCE (OP_EQ) r243)
     (REDUCE (OP_NE) r243)
     (REDUCE (OP_LAND) r243)
     (REDUCE (OP_LOR) r243)
     (REDUCE (SEMICOLON) r243)
     (REDUCE (COMMA) r243)
     (REDUCE (RPAREN) r243)
     (REDUCE (RBRACK) r243)
     (REDUCE (RCURLY) r243)
     (REDUCE (LESS_THAN) r243)
     (REDUCE (GREATER_THAN) r243)
     (REDUCE (COLON) r243)
     (REDUCE (PIPE) r243)
     (REDUCE (QMARK) r243)
     (REDUCE (AMPERSAND) r243)
     (REDUCE (CARROT) r243))
    (STATE
     s126
     (COMMENT EqualityExpression "=>" RelationalExpression ".")
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      LESS_THAN
      ShiftExpression)
     (SHIFT (INSTANCEOF) s325)
     (SHIFT (OP_GE) s326)
     (SHIFT (OP_LE) s327)
     (REDUCE (OP_EQ) r249)
     (REDUCE (OP_NE) r249)
     (REDUCE (OP_LAND) r249)
     (REDUCE (OP_LOR) r249)
     (REDUCE (SEMICOLON) r249)
     (REDUCE (COMMA) r249)
     (REDUCE (RPAREN) r249)
     (REDUCE (RBRACK) r249)
     (REDUCE (RCURLY) r249)
     (SHIFT (LESS_THAN) s328)
     (SHIFT (GREATER_THAN) s329)
     (REDUCE (COLON) r249)
     (REDUCE (PIPE) r249)
     (REDUCE (QMARK) r249)
     (REDUCE (AMPERSAND) r249)
     (REDUCE (CARROT) r249))
    (STATE
     s127
     (COMMENT AndExpression "=>" EqualityExpression ".")
     (COMMENT
      EqualityExpression
      "=>"
      EqualityExpression
      "."
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      EqualityExpression
      "."
      OP_EQ
      RelationalExpression)
     (SHIFT (OP_EQ) s322)
     (SHIFT (OP_NE) s323)
     (REDUCE (OP_LAND) r252)
     (REDUCE (OP_LOR) r252)
     (REDUCE (SEMICOLON) r252)
     (REDUCE (COMMA) r252)
     (REDUCE (RPAREN) r252)
     (REDUCE (RBRACK) r252)
     (REDUCE (RCURLY) r252)
     (REDUCE (COLON) r252)
     (REDUCE (PIPE) r252)
     (REDUCE (QMARK) r252)
     (REDUCE (AMPERSAND) r252)
     (REDUCE (CARROT) r252))
    (STATE
     s128
     (COMMENT ExclusiveOrExpression "=>" AndExpression ".")
     (COMMENT
      AndExpression
      "=>"
      AndExpression
      "."
      AMPERSAND
      EqualityExpression)
     (REDUCE (OP_LAND) r254)
     (REDUCE (OP_LOR) r254)
     (REDUCE (SEMICOLON) r254)
     (REDUCE (COMMA) r254)
     (REDUCE (RPAREN) r254)
     (REDUCE (RBRACK) r254)
     (REDUCE (RCURLY) r254)
     (REDUCE (COLON) r254)
     (REDUCE (PIPE) r254)
     (REDUCE (QMARK) r254)
     (SHIFT (AMPERSAND) s320)
     (REDUCE (CARROT) r254))
    (STATE
     s129
     (COMMENT InclusiveOrExpression "=>" ExclusiveOrExpression ".")
     (COMMENT
      ExclusiveOrExpression
      "=>"
      ExclusiveOrExpression
      "."
      CARROT
      AndExpression)
     (REDUCE (OP_LAND) r256)
     (REDUCE (OP_LOR) r256)
     (REDUCE (SEMICOLON) r256)
     (REDUCE (COMMA) r256)
     (REDUCE (RPAREN) r256)
     (REDUCE (RBRACK) r256)
     (REDUCE (RCURLY) r256)
     (REDUCE (COLON) r256)
     (REDUCE (PIPE) r256)
     (REDUCE (QMARK) r256)
     (SHIFT (CARROT) s318))
    (STATE
     s130
     (COMMENT ConditionalAndExpression "=>" InclusiveOrExpression ".")
     (COMMENT
      InclusiveOrExpression
      "=>"
      InclusiveOrExpression
      "."
      PIPE
      ExclusiveOrExpression)
     (REDUCE (OP_LAND) r258)
     (REDUCE (OP_LOR) r258)
     (REDUCE (SEMICOLON) r258)
     (REDUCE (COMMA) r258)
     (REDUCE (RPAREN) r258)
     (REDUCE (RBRACK) r258)
     (REDUCE (RCURLY) r258)
     (REDUCE (COLON) r258)
     (SHIFT (PIPE) s316)
     (REDUCE (QMARK) r258))
    (STATE
     s131
     (COMMENT ConditionalOrExpression "=>" ConditionalAndExpression ".")
     (COMMENT
      ConditionalAndExpression
      "=>"
      ConditionalAndExpression
      "."
      OP_LAND
      InclusiveOrExpression)
     (SHIFT (OP_LAND) s314)
     (REDUCE (OP_LOR) r260)
     (REDUCE (SEMICOLON) r260)
     (REDUCE (COMMA) r260)
     (REDUCE (RPAREN) r260)
     (REDUCE (RBRACK) r260)
     (REDUCE (RCURLY) r260)
     (REDUCE (COLON) r260)
     (REDUCE (QMARK) r260))
    (STATE
     s132
     (COMMENT
      ConditionalExpression
      "=>"
      ConditionalOrExpression
      "."
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" ConditionalOrExpression ".")
     (COMMENT
      ConditionalOrExpression
      "=>"
      ConditionalOrExpression
      "."
      OP_LOR
      ConditionalAndExpression)
     (SHIFT (OP_LOR) s308)
     (REDUCE (SEMICOLON) r262)
     (REDUCE (COMMA) r262)
     (REDUCE (RPAREN) r262)
     (REDUCE (RBRACK) r262)
     (REDUCE (RCURLY) r262)
     (REDUCE (COLON) r262)
     (SHIFT (QMARK) s309))
    (STATE
     s133
     (COMMENT AssignmentExpression "=>" ConditionalExpression ".")
     (REDUCE () r264))
    (STATE
     s134
     (COMMENT Expression "=>" AssignmentExpression ".")
     (REDUCE () r278))
    (STATE
     s135
     (COMMENT ExpressionStatement "=>" Expression ".")
     (REDUCE () r131))
    (STATE
     s136
     (COMMENT JumpStatement "=>" BREAK "." SEMICOLON)
     (COMMENT JumpStatement "=>" BREAK "." IDENTIFIER SEMICOLON)
     (SHIFT (IDENTIFIER) s305)
     (SHIFT (SEMICOLON) s306))
    (STATE
     s137
     (COMMENT ConstantExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT LabelStatement "=>" CASE "." ConstantExpression COLON)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s302)
     (GOTO ConstantExpression s303))
    (STATE
     s138
     (COMMENT JumpStatement "=>" CONTINUE "." SEMICOLON)
     (COMMENT JumpStatement "=>" CONTINUE "." IDENTIFIER SEMICOLON)
     (SHIFT (IDENTIFIER) s299)
     (SHIFT (SEMICOLON) s300))
    (STATE
     s139
     (COMMENT LabelStatement "=>" DEFAULT "." COLON)
     (SHIFT (COLON) s298))
    (STATE
     s140
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches Finally)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches)
     (COMMENT GuardingStatement "=>" "." TRY Block Finally)
     (COMMENT
      GuardingStatement
      "=>"
      "."
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT JumpStatement "=>" "." THROW Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE IDENTIFIER SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK IDENTIFIER SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      DO
      "."
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT LabelStatement "=>" "." DEFAULT COLON)
     (COMMENT LabelStatement "=>" "." CASE ConstantExpression COLON)
     (COMMENT LabelStatement "=>" "." IDENTIFIER COLON)
     (COMMENT EmptyStatement "=>" "." SEMICOLON)
     (COMMENT Statement "=>" "." Block)
     (COMMENT Statement "=>" "." GuardingStatement)
     (COMMENT Statement "=>" "." JumpStatement)
     (COMMENT Statement "=>" "." IterationStatement)
     (COMMENT Statement "=>" "." SelectionStatement)
     (COMMENT Statement "=>" "." ExpressionStatement SEMICOLON)
     (COMMENT Statement "=>" "." LabelStatement)
     (COMMENT Statement "=>" "." EmptyStatement)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BREAK) s136)
     (SHIFT (BYTE) s52)
     (SHIFT (CASE) s137)
     (SHIFT (CHAR) s53)
     (SHIFT (CONTINUE) s138)
     (SHIFT (DEFAULT) s139)
     (SHIFT (DO) s140)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (FOR) s142)
     (SHIFT (IF) s143)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (RETURN) s146)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (SWITCH) s148)
     (SHIFT (SYNCHRONIZED) s149)
     (SHIFT (THIS) s150)
     (SHIFT (THROW) s151)
     (SHIFT (TRY) s152)
     (SHIFT (VOID) s59)
     (SHIFT (WHILE) s153)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s156)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s159)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s86)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO Block s91)
     (GOTO Statement s292)
     (GOTO EmptyStatement s96)
     (GOTO LabelStatement s97)
     (GOTO ExpressionStatement s98)
     (GOTO SelectionStatement s99)
     (GOTO IterationStatement s100)
     (GOTO JumpStatement s101)
     (GOTO GuardingStatement s102)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s141
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      FINAL
      "."
      TypeSpecifier
      VariableDeclarators
      SEMICOLON)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (GOTO TypeSpecifier s273)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50))
    (STATE
     s142
     (COMMENT
      IterationStatement
      "=>"
      FOR
      "."
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      FOR
      "."
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (SHIFT (LPAREN) s254))
    (STATE
     s143
     (COMMENT
      SelectionStatement
      "=>"
      IF
      "."
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      IF
      "."
      LPAREN
      Expression
      RPAREN
      Statement)
     (SHIFT (LPAREN) s248))
    (STATE
     s144
     (COMMENT ArrayAllocationExpression "=>" NEW "." TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" NEW "." TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" NEW "." TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" NEW "." TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      NEW
      "."
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (GOTO TypeName s232)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50))
    (STATE s145 (COMMENT SpecialName "=>" JNULL ".") (REDUCE () r190))
    (STATE
     s146
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT JumpStatement "=>" RETURN "." SEMICOLON)
     (COMMENT JumpStatement "=>" RETURN "." Expression SEMICOLON)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s230)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s229))
    (STATE s147 (COMMENT SpecialName "=>" SUPER ".") (REDUCE () r189))
    (STATE
     s148
     (COMMENT
      SelectionStatement
      "=>"
      SWITCH
      "."
      LPAREN
      Expression
      RPAREN
      Block)
     (SHIFT (LPAREN) s225))
    (STATE
     s149
     (COMMENT
      GuardingStatement
      "=>"
      SYNCHRONIZED
      "."
      LPAREN
      Expression
      RPAREN
      Statement)
     (SHIFT (LPAREN) s221))
    (STATE s150 (COMMENT SpecialName "=>" THIS ".") (REDUCE () r188))
    (STATE
     s151
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT JumpStatement "=>" THROW "." Expression SEMICOLON)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s219))
    (STATE
     s152
     (COMMENT GuardingStatement "=>" TRY "." Block Catches Finally)
     (COMMENT GuardingStatement "=>" TRY "." Block Catches)
     (COMMENT GuardingStatement "=>" TRY "." Block Finally)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (SHIFT (LCURLY) s86)
     (GOTO Block s203))
    (STATE
     s153
     (COMMENT
      IterationStatement
      "=>"
      WHILE
      "."
      LPAREN
      Expression
      RPAREN
      Statement)
     (SHIFT (LPAREN) s199))
    (STATE
     s154
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" OP_INC "." UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s175)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s198)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121))
    (STATE
     s155
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" OP_DEC "." UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s175)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s197)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121))
    (STATE
     s156
     (COMMENT LabelStatement "=>" IDENTIFIER "." COLON)
     (COMMENT QualifiedName "=>" IDENTIFIER ".")
     (REDUCE (INSTANCEOF) r36)
     (REDUCE (OP_INC) r36)
     (REDUCE (OP_DEC) r36)
     (REDUCE (OP_SHL) r36)
     (REDUCE (OP_SHR) r36)
     (REDUCE (OP_SHRR) r36)
     (REDUCE (OP_GE) r36)
     (REDUCE (OP_LE) r36)
     (REDUCE (OP_EQ) r36)
     (REDUCE (OP_NE) r36)
     (REDUCE (OP_LAND) r36)
     (REDUCE (OP_LOR) r36)
     (REDUCE (OP_DIM) r36)
     (REDUCE (ASS_MUL) r36)
     (REDUCE (ASS_DIV) r36)
     (REDUCE (ASS_MOD) r36)
     (REDUCE (ASS_ADD) r36)
     (REDUCE (ASS_SUB) r36)
     (REDUCE (ASS_SHL) r36)
     (REDUCE (ASS_SHR) r36)
     (REDUCE (ASS_SHRR) r36)
     (REDUCE (ASS_AND) r36)
     (REDUCE (ASS_XOR) r36)
     (REDUCE (ASS_OR) r36)
     (REDUCE (IDENTIFIER) r36)
     (REDUCE (SEMICOLON) r36)
     (REDUCE (LPAREN) r36)
     (REDUCE (LBRACK) r36)
     (REDUCE (EQUAL_SIGN) r36)
     (REDUCE (DOT) r36)
     (REDUCE (PLUS) r36)
     (REDUCE (STAR) r36)
     (REDUCE (FORWARD_SLASH) r36)
     (REDUCE (MINUS) r36)
     (REDUCE (PERCENT) r36)
     (REDUCE (LESS_THAN) r36)
     (REDUCE (GREATER_THAN) r36)
     (SHIFT (COLON) s196)
     (REDUCE (PIPE) r36)
     (REDUCE (QMARK) r36)
     (REDUCE (AMPERSAND) r36)
     (REDUCE (CARROT) r36))
    (STATE
     s157
     (COMMENT ComplexPrimaryNoParenthesis "=>" LITERAL ".")
     (REDUCE () r171))
    (STATE
     s158
     (COMMENT ComplexPrimaryNoParenthesis "=>" BOOLLIT ".")
     (REDUCE () r172))
    (STATE s159 (COMMENT EmptyStatement "=>" SEMICOLON ".") (REDUCE () r127))
    (STATE
     s160
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT ClassTypeExpression "=>" "." QualifiedName Dims)
     (COMMENT PrimitiveTypeExpression "=>" "." PrimitiveType Dims)
     (COMMENT PrimitiveTypeExpression "=>" "." PrimitiveType)
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      "."
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      "."
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      "."
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" LPAREN "." Expression RPAREN)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s166)
     (GOTO QualifiedName s167)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO PrimitiveTypeExpression s168)
     (GOTO ClassTypeExpression s169)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s170))
    (STATE s161 (COMMENT Block "=>" LCURLY RCURLY ".") (REDUCE () r112))
    (STATE
     s162
     (COMMENT ArithmeticUnaryOperator "=>" PLUS ".")
     (REDUCE () r223))
    (STATE
     s163
     (COMMENT ArithmeticUnaryOperator "=>" MINUS ".")
     (REDUCE () r224))
    (STATE s164 (COMMENT LogicalUnaryOperator "=>" BANG ".") (REDUCE () r222))
    (STATE
     s165
     (COMMENT LogicalUnaryOperator "=>" TWIDDLE ".")
     (REDUCE () r221))
    (STATE
     s166
     (COMMENT PrimitiveTypeExpression "=>" PrimitiveType "." Dims)
     (COMMENT PrimitiveTypeExpression "=>" PrimitiveType ".")
     (COMMENT Dims "=>" "." Dims OP_DIM)
     (COMMENT Dims "=>" "." OP_DIM)
     (COMMENT FieldAccess "=>" PrimitiveType "." DOT CLASS)
     (SHIFT (OP_DIM) s193)
     (REDUCE (RPAREN) r229)
     (SHIFT (DOT) s185)
     (GOTO Dims s195))
    (STATE
     s167
     (COMMENT ClassTypeExpression "=>" QualifiedName "." Dims)
     (COMMENT Dims "=>" "." Dims OP_DIM)
     (COMMENT Dims "=>" "." OP_DIM)
     (COMMENT
      NewAllocationExpression
      "=>"
      QualifiedName
      "."
      DOT
      PlainNewAllocationExpression)
     (COMMENT MethodAccess "=>" QualifiedName ".")
     (COMMENT FieldAccess "=>" QualifiedName "." DOT CLASS)
     (COMMENT FieldAccess "=>" QualifiedName "." DOT THIS)
     (COMMENT ArrayAccess "=>" QualifiedName "." LBRACK Expression RBRACK)
     (COMMENT PrimaryExpression "=>" QualifiedName ".")
     (COMMENT QualifiedName "=>" QualifiedName "." DOT IDENTIFIER)
     (REDUCE (INSTANCEOF) r164)
     (REDUCE (OP_INC) r164)
     (REDUCE (OP_DEC) r164)
     (REDUCE (OP_SHL) r164)
     (REDUCE (OP_SHR) r164)
     (REDUCE (OP_SHRR) r164)
     (REDUCE (OP_GE) r164)
     (REDUCE (OP_LE) r164)
     (REDUCE (OP_EQ) r164)
     (REDUCE (OP_NE) r164)
     (REDUCE (OP_LAND) r164)
     (REDUCE (OP_LOR) r164)
     (SHIFT (OP_DIM) s193)
     (REDUCE (ASS_MUL) r164)
     (REDUCE (ASS_DIV) r164)
     (REDUCE (ASS_MOD) r164)
     (REDUCE (ASS_ADD) r164)
     (REDUCE (ASS_SUB) r164)
     (REDUCE (ASS_SHL) r164)
     (REDUCE (ASS_SHR) r164)
     (REDUCE (ASS_SHRR) r164)
     (REDUCE (ASS_AND) r164)
     (REDUCE (ASS_XOR) r164)
     (REDUCE (ASS_OR) r164)
     (REDUCE (LPAREN) r187)
     (REDUCE (RPAREN) r164)
     (SHIFT (LBRACK) s178)
     (REDUCE (EQUAL_SIGN) r164)
     (SHIFT (DOT) s179)
     (REDUCE (PLUS) r164)
     (REDUCE (STAR) r164)
     (REDUCE (FORWARD_SLASH) r164)
     (REDUCE (MINUS) r164)
     (REDUCE (PERCENT) r164)
     (REDUCE (LESS_THAN) r164)
     (REDUCE (GREATER_THAN) r164)
     (REDUCE (PIPE) r164)
     (REDUCE (QMARK) r164)
     (REDUCE (AMPERSAND) r164)
     (REDUCE (CARROT) r164)
     (GOTO Dims s192))
    (STATE
     s168
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      PrimitiveTypeExpression
      "."
      RPAREN
      CastExpression)
     (SHIFT (RPAREN) s190))
    (STATE
     s169
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      ClassTypeExpression
      "."
      RPAREN
      CastExpression)
     (SHIFT (RPAREN) s187))
    (STATE
     s170
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      Expression
      "."
      RPAREN
      LogicalUnaryExpression)
     (COMMENT ComplexPrimary "=>" LPAREN Expression "." RPAREN)
     (SHIFT (RPAREN) s171))
    (STATE
     s171
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      Expression
      RPAREN
      "."
      LogicalUnaryExpression)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" LPAREN Expression RPAREN ".")
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (REDUCE (INSTANCEOF) r169)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (REDUCE (OP_INC) r169)
     (REDUCE (OP_DEC) r169)
     (REDUCE (OP_SHL) r169)
     (REDUCE (OP_SHR) r169)
     (REDUCE (OP_SHRR) r169)
     (REDUCE (OP_GE) r169)
     (REDUCE (OP_LE) r169)
     (REDUCE (OP_EQ) r169)
     (REDUCE (OP_NE) r169)
     (REDUCE (OP_LAND) r169)
     (REDUCE (OP_LOR) r169)
     (REDUCE (ASS_MUL) r169)
     (REDUCE (ASS_DIV) r169)
     (REDUCE (ASS_MOD) r169)
     (REDUCE (ASS_ADD) r169)
     (REDUCE (ASS_SUB) r169)
     (REDUCE (ASS_SHL) r169)
     (REDUCE (ASS_SHR) r169)
     (REDUCE (ASS_SHRR) r169)
     (REDUCE (ASS_AND) r169)
     (REDUCE (ASS_XOR) r169)
     (REDUCE (ASS_OR) r169)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (REDUCE (SEMICOLON) r169)
     (REDUCE (COMMA) r169)
     (SHIFT (LPAREN) s175)
     (REDUCE (RPAREN) r169)
     (REDUCE (LBRACK) r169)
     (REDUCE (RBRACK) r169)
     (REDUCE (RCURLY) r169)
     (REDUCE (EQUAL_SIGN) r169)
     (REDUCE (DOT) r169)
     (REDUCE (PLUS) r169)
     (REDUCE (STAR) r169)
     (REDUCE (FORWARD_SLASH) r169)
     (REDUCE (MINUS) r169)
     (REDUCE (PERCENT) r169)
     (REDUCE (LESS_THAN) r169)
     (REDUCE (GREATER_THAN) r169)
     (REDUCE (COLON) r169)
     (REDUCE (PIPE) r169)
     (REDUCE (QMARK) r169)
     (REDUCE (AMPERSAND) r169)
     (REDUCE (CARROT) r169)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO LogicalUnaryExpression s174)
     (GOTO LogicalUnaryOperator s120))
    (STATE
     s172
     (COMMENT FieldAccess "=>" PrimitiveType "." DOT CLASS)
     (SHIFT (DOT) s185))
    (STATE
     s173
     (COMMENT
      NewAllocationExpression
      "=>"
      QualifiedName
      "."
      DOT
      PlainNewAllocationExpression)
     (COMMENT MethodAccess "=>" QualifiedName ".")
     (COMMENT FieldAccess "=>" QualifiedName "." DOT CLASS)
     (COMMENT FieldAccess "=>" QualifiedName "." DOT THIS)
     (COMMENT ArrayAccess "=>" QualifiedName "." LBRACK Expression RBRACK)
     (COMMENT PrimaryExpression "=>" QualifiedName ".")
     (COMMENT QualifiedName "=>" QualifiedName "." DOT IDENTIFIER)
     (REDUCE (INSTANCEOF) r164)
     (REDUCE (OP_INC) r164)
     (REDUCE (OP_DEC) r164)
     (REDUCE (OP_SHL) r164)
     (REDUCE (OP_SHR) r164)
     (REDUCE (OP_SHRR) r164)
     (REDUCE (OP_GE) r164)
     (REDUCE (OP_LE) r164)
     (REDUCE (OP_EQ) r164)
     (REDUCE (OP_NE) r164)
     (REDUCE (OP_LAND) r164)
     (REDUCE (OP_LOR) r164)
     (REDUCE (ASS_MUL) r164)
     (REDUCE (ASS_DIV) r164)
     (REDUCE (ASS_MOD) r164)
     (REDUCE (ASS_ADD) r164)
     (REDUCE (ASS_SUB) r164)
     (REDUCE (ASS_SHL) r164)
     (REDUCE (ASS_SHR) r164)
     (REDUCE (ASS_SHRR) r164)
     (REDUCE (ASS_AND) r164)
     (REDUCE (ASS_XOR) r164)
     (REDUCE (ASS_OR) r164)
     (REDUCE (SEMICOLON) r164)
     (REDUCE (COMMA) r164)
     (REDUCE (LPAREN) r187)
     (REDUCE (RPAREN) r164)
     (SHIFT (LBRACK) s178)
     (REDUCE (RBRACK) r164)
     (REDUCE (RCURLY) r164)
     (REDUCE (EQUAL_SIGN) r164)
     (SHIFT (DOT) s179)
     (REDUCE (PLUS) r164)
     (REDUCE (STAR) r164)
     (REDUCE (FORWARD_SLASH) r164)
     (REDUCE (MINUS) r164)
     (REDUCE (PERCENT) r164)
     (REDUCE (LESS_THAN) r164)
     (REDUCE (GREATER_THAN) r164)
     (REDUCE (COLON) r164)
     (REDUCE (PIPE) r164)
     (REDUCE (QMARK) r164)
     (REDUCE (AMPERSAND) r164)
     (REDUCE (CARROT) r164))
    (STATE
     s174
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression
      ".")
     (REDUCE () r228))
    (STATE
     s175
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" LPAREN "." Expression RPAREN)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s176))
    (STATE
     s176
     (COMMENT ComplexPrimary "=>" LPAREN Expression "." RPAREN)
     (SHIFT (RPAREN) s177))
    (STATE
     s177
     (COMMENT ComplexPrimary "=>" LPAREN Expression RPAREN ".")
     (REDUCE () r169))
    (STATE
     s178
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" QualifiedName LBRACK "." Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s183))
    (STATE
     s179
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      QualifiedName
      DOT
      "."
      PlainNewAllocationExpression)
     (COMMENT FieldAccess "=>" QualifiedName DOT "." CLASS)
     (COMMENT FieldAccess "=>" QualifiedName DOT "." THIS)
     (COMMENT QualifiedName "=>" QualifiedName DOT "." IDENTIFIER)
     (SHIFT (CLASS) s181)
     (SHIFT (NEW) s144)
     (SHIFT (THIS) s182)
     (SHIFT (IDENTIFIER) s32)
     (GOTO PlainNewAllocationExpression s180)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115))
    (STATE
     s180
     (COMMENT
      NewAllocationExpression
      "=>"
      QualifiedName
      DOT
      PlainNewAllocationExpression
      ".")
     (REDUCE () r194))
    (STATE
     s181
     (COMMENT FieldAccess "=>" QualifiedName DOT CLASS ".")
     (REDUCE () r181))
    (STATE
     s182
     (COMMENT FieldAccess "=>" QualifiedName DOT THIS ".")
     (REDUCE () r180))
    (STATE
     s183
     (COMMENT ArrayAccess "=>" QualifiedName LBRACK Expression "." RBRACK)
     (SHIFT (RBRACK) s184))
    (STATE
     s184
     (COMMENT ArrayAccess "=>" QualifiedName LBRACK Expression RBRACK ".")
     (REDUCE () r176))
    (STATE
     s185
     (COMMENT FieldAccess "=>" PrimitiveType DOT "." CLASS)
     (SHIFT (CLASS) s186))
    (STATE
     s186
     (COMMENT FieldAccess "=>" PrimitiveType DOT CLASS ".")
     (REDUCE () r182))
    (STATE
     s187
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      ClassTypeExpression
      RPAREN
      "."
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s189))
    (STATE
     s188
     (COMMENT CastExpression "=>" UnaryExpression ".")
     (REDUCE () r225))
    (STATE
     s189
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression
      ".")
     (REDUCE () r227))
    (STATE
     s190
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      "."
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s191))
    (STATE
     s191
     (COMMENT
      CastExpression
      "=>"
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression
      ".")
     (REDUCE () r226))
    (STATE
     s192
     (COMMENT ClassTypeExpression "=>" QualifiedName Dims ".")
     (COMMENT Dims "=>" Dims "." OP_DIM)
     (SHIFT (OP_DIM) s194)
     (REDUCE (RPAREN) r231))
    (STATE s193 (COMMENT Dims "=>" OP_DIM ".") (REDUCE () r209))
    (STATE s194 (COMMENT Dims "=>" Dims OP_DIM ".") (REDUCE () r210))
    (STATE
     s195
     (COMMENT PrimitiveTypeExpression "=>" PrimitiveType Dims ".")
     (COMMENT Dims "=>" Dims "." OP_DIM)
     (SHIFT (OP_DIM) s194)
     (REDUCE (RPAREN) r230))
    (STATE
     s196
     (COMMENT LabelStatement "=>" IDENTIFIER COLON ".")
     (REDUCE () r128))
    (STATE
     s197
     (COMMENT UnaryExpression "=>" OP_DEC UnaryExpression ".")
     (REDUCE () r216))
    (STATE
     s198
     (COMMENT UnaryExpression "=>" OP_INC UnaryExpression ".")
     (REDUCE () r215))
    (STATE
     s199
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT
      IterationStatement
      "=>"
      WHILE
      LPAREN
      "."
      Expression
      RPAREN
      Statement)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s200))
    (STATE
     s200
     (COMMENT
      IterationStatement
      "=>"
      WHILE
      LPAREN
      Expression
      "."
      RPAREN
      Statement)
     (SHIFT (RPAREN) s201))
    (STATE
     s201
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches Finally)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches)
     (COMMENT GuardingStatement "=>" "." TRY Block Finally)
     (COMMENT
      GuardingStatement
      "=>"
      "."
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT JumpStatement "=>" "." THROW Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE IDENTIFIER SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK IDENTIFIER SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      WHILE
      LPAREN
      Expression
      RPAREN
      "."
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT LabelStatement "=>" "." DEFAULT COLON)
     (COMMENT LabelStatement "=>" "." CASE ConstantExpression COLON)
     (COMMENT LabelStatement "=>" "." IDENTIFIER COLON)
     (COMMENT EmptyStatement "=>" "." SEMICOLON)
     (COMMENT Statement "=>" "." Block)
     (COMMENT Statement "=>" "." GuardingStatement)
     (COMMENT Statement "=>" "." JumpStatement)
     (COMMENT Statement "=>" "." IterationStatement)
     (COMMENT Statement "=>" "." SelectionStatement)
     (COMMENT Statement "=>" "." ExpressionStatement SEMICOLON)
     (COMMENT Statement "=>" "." LabelStatement)
     (COMMENT Statement "=>" "." EmptyStatement)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BREAK) s136)
     (SHIFT (BYTE) s52)
     (SHIFT (CASE) s137)
     (SHIFT (CHAR) s53)
     (SHIFT (CONTINUE) s138)
     (SHIFT (DEFAULT) s139)
     (SHIFT (DO) s140)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (FOR) s142)
     (SHIFT (IF) s143)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (RETURN) s146)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (SWITCH) s148)
     (SHIFT (SYNCHRONIZED) s149)
     (SHIFT (THIS) s150)
     (SHIFT (THROW) s151)
     (SHIFT (TRY) s152)
     (SHIFT (VOID) s59)
     (SHIFT (WHILE) s153)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s156)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s159)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s86)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO Block s91)
     (GOTO Statement s202)
     (GOTO EmptyStatement s96)
     (GOTO LabelStatement s97)
     (GOTO ExpressionStatement s98)
     (GOTO SelectionStatement s99)
     (GOTO IterationStatement s100)
     (GOTO JumpStatement s101)
     (GOTO GuardingStatement s102)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s202
     (COMMENT
      IterationStatement
      "=>"
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement
      ".")
     (REDUCE () r135))
    (STATE
     s203
     (COMMENT Finally "=>" "." FINALLY Block)
     (COMMENT CatchHeader "=>" "." CATCH LPAREN TypeSpecifier RPAREN)
     (COMMENT
      CatchHeader
      "=>"
      "."
      CATCH
      LPAREN
      TypeSpecifier
      IDENTIFIER
      RPAREN)
     (COMMENT Catch "=>" "." CatchHeader Block)
     (COMMENT Catches "=>" "." Catches Catch)
     (COMMENT Catches "=>" "." Catch)
     (COMMENT GuardingStatement "=>" TRY Block "." Catches Finally)
     (COMMENT GuardingStatement "=>" TRY Block "." Catches)
     (COMMENT GuardingStatement "=>" TRY Block "." Finally)
     (SHIFT (CATCH) s208)
     (SHIFT (FINALLY) s209)
     (GOTO Catches s204)
     (GOTO Catch s205)
     (GOTO CatchHeader s206)
     (GOTO Finally s207))
    (STATE
     s204
     (COMMENT Finally "=>" "." FINALLY Block)
     (COMMENT CatchHeader "=>" "." CATCH LPAREN TypeSpecifier RPAREN)
     (COMMENT
      CatchHeader
      "=>"
      "."
      CATCH
      LPAREN
      TypeSpecifier
      IDENTIFIER
      RPAREN)
     (COMMENT Catch "=>" "." CatchHeader Block)
     (COMMENT Catches "=>" Catches "." Catch)
     (COMMENT GuardingStatement "=>" TRY Block Catches "." Finally)
     (COMMENT GuardingStatement "=>" TRY Block Catches ".")
     (REDUCE (BOOLEAN) r156)
     (REDUCE (BREAK) r156)
     (REDUCE (BYTE) r156)
     (REDUCE (CASE) r156)
     (SHIFT (CATCH) s208)
     (REDUCE (CHAR) r156)
     (REDUCE (CONTINUE) r156)
     (REDUCE (DEFAULT) r156)
     (REDUCE (DO) r156)
     (REDUCE (DOUBLE) r156)
     (REDUCE (ELSE) r156)
     (REDUCE (FINAL) r156)
     (SHIFT (FINALLY) s209)
     (REDUCE (FLOAT) r156)
     (REDUCE (FOR) r156)
     (REDUCE (IF) r156)
     (REDUCE (INT) r156)
     (REDUCE (LONG) r156)
     (REDUCE (NEW) r156)
     (REDUCE (JNULL) r156)
     (REDUCE (RETURN) r156)
     (REDUCE (SHORT) r156)
     (REDUCE (SUPER) r156)
     (REDUCE (SWITCH) r156)
     (REDUCE (SYNCHRONIZED) r156)
     (REDUCE (THIS) r156)
     (REDUCE (THROW) r156)
     (REDUCE (TRY) r156)
     (REDUCE (VOID) r156)
     (REDUCE (WHILE) r156)
     (REDUCE (OP_INC) r156)
     (REDUCE (OP_DEC) r156)
     (REDUCE (IDENTIFIER) r156)
     (REDUCE (LITERAL) r156)
     (REDUCE (BOOLLIT) r156)
     (REDUCE (SEMICOLON) r156)
     (REDUCE (LPAREN) r156)
     (REDUCE (LCURLY) r156)
     (REDUCE (RCURLY) r156)
     (REDUCE (PLUS) r156)
     (REDUCE (MINUS) r156)
     (REDUCE (BANG) r156)
     (REDUCE (TWIDDLE) r156)
     (GOTO Catch s217)
     (GOTO CatchHeader s206)
     (GOTO Finally s218))
    (STATE s205 (COMMENT Catches "=>" Catch ".") (REDUCE () r158))
    (STATE
     s206
     (COMMENT Catch "=>" CatchHeader "." Block)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (SHIFT (LCURLY) s86)
     (GOTO Block s216))
    (STATE
     s207
     (COMMENT GuardingStatement "=>" TRY Block Finally ".")
     (REDUCE () r155))
    (STATE
     s208
     (COMMENT CatchHeader "=>" CATCH "." LPAREN TypeSpecifier RPAREN)
     (COMMENT
      CatchHeader
      "=>"
      CATCH
      "."
      LPAREN
      TypeSpecifier
      IDENTIFIER
      RPAREN)
     (SHIFT (LPAREN) s211))
    (STATE
     s209
     (COMMENT Finally "=>" FINALLY "." Block)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (SHIFT (LCURLY) s86)
     (GOTO Block s210))
    (STATE s210 (COMMENT Finally "=>" FINALLY Block ".") (REDUCE () r163))
    (STATE
     s211
     (COMMENT CatchHeader "=>" CATCH LPAREN "." TypeSpecifier RPAREN)
     (COMMENT
      CatchHeader
      "=>"
      CATCH
      LPAREN
      "."
      TypeSpecifier
      IDENTIFIER
      RPAREN)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (GOTO TypeSpecifier s212)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50))
    (STATE
     s212
     (COMMENT CatchHeader "=>" CATCH LPAREN TypeSpecifier "." RPAREN)
     (COMMENT
      CatchHeader
      "=>"
      CATCH
      LPAREN
      TypeSpecifier
      "."
      IDENTIFIER
      RPAREN)
     (SHIFT (IDENTIFIER) s213)
     (SHIFT (RPAREN) s214))
    (STATE
     s213
     (COMMENT
      CatchHeader
      "=>"
      CATCH
      LPAREN
      TypeSpecifier
      IDENTIFIER
      "."
      RPAREN)
     (SHIFT (RPAREN) s215))
    (STATE
     s214
     (COMMENT CatchHeader "=>" CATCH LPAREN TypeSpecifier RPAREN ".")
     (REDUCE () r162))
    (STATE
     s215
     (COMMENT
      CatchHeader
      "=>"
      CATCH
      LPAREN
      TypeSpecifier
      IDENTIFIER
      RPAREN
      ".")
     (REDUCE () r161))
    (STATE s216 (COMMENT Catch "=>" CatchHeader Block ".") (REDUCE () r160))
    (STATE s217 (COMMENT Catches "=>" Catches Catch ".") (REDUCE () r159))
    (STATE
     s218
     (COMMENT GuardingStatement "=>" TRY Block Catches Finally ".")
     (REDUCE () r157))
    (STATE
     s219
     (COMMENT JumpStatement "=>" THROW Expression "." SEMICOLON)
     (SHIFT (SEMICOLON) s220))
    (STATE
     s220
     (COMMENT JumpStatement "=>" THROW Expression SEMICOLON ".")
     (REDUCE () r153))
    (STATE
     s221
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT
      GuardingStatement
      "=>"
      SYNCHRONIZED
      LPAREN
      "."
      Expression
      RPAREN
      Statement)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s222))
    (STATE
     s222
     (COMMENT
      GuardingStatement
      "=>"
      SYNCHRONIZED
      LPAREN
      Expression
      "."
      RPAREN
      Statement)
     (SHIFT (RPAREN) s223))
    (STATE
     s223
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches Finally)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches)
     (COMMENT GuardingStatement "=>" "." TRY Block Finally)
     (COMMENT
      GuardingStatement
      "=>"
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      "."
      Statement)
     (COMMENT
      GuardingStatement
      "=>"
      "."
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT JumpStatement "=>" "." THROW Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE IDENTIFIER SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK IDENTIFIER SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT LabelStatement "=>" "." DEFAULT COLON)
     (COMMENT LabelStatement "=>" "." CASE ConstantExpression COLON)
     (COMMENT LabelStatement "=>" "." IDENTIFIER COLON)
     (COMMENT EmptyStatement "=>" "." SEMICOLON)
     (COMMENT Statement "=>" "." Block)
     (COMMENT Statement "=>" "." GuardingStatement)
     (COMMENT Statement "=>" "." JumpStatement)
     (COMMENT Statement "=>" "." IterationStatement)
     (COMMENT Statement "=>" "." SelectionStatement)
     (COMMENT Statement "=>" "." ExpressionStatement SEMICOLON)
     (COMMENT Statement "=>" "." LabelStatement)
     (COMMENT Statement "=>" "." EmptyStatement)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BREAK) s136)
     (SHIFT (BYTE) s52)
     (SHIFT (CASE) s137)
     (SHIFT (CHAR) s53)
     (SHIFT (CONTINUE) s138)
     (SHIFT (DEFAULT) s139)
     (SHIFT (DO) s140)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (FOR) s142)
     (SHIFT (IF) s143)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (RETURN) s146)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (SWITCH) s148)
     (SHIFT (SYNCHRONIZED) s149)
     (SHIFT (THIS) s150)
     (SHIFT (THROW) s151)
     (SHIFT (TRY) s152)
     (SHIFT (VOID) s59)
     (SHIFT (WHILE) s153)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s156)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s159)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s86)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO Block s91)
     (GOTO Statement s224)
     (GOTO EmptyStatement s96)
     (GOTO LabelStatement s97)
     (GOTO ExpressionStatement s98)
     (GOTO SelectionStatement s99)
     (GOTO IterationStatement s100)
     (GOTO JumpStatement s101)
     (GOTO GuardingStatement s102)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s224
     (COMMENT
      GuardingStatement
      "=>"
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement
      ".")
     (REDUCE () r154))
    (STATE
     s225
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT
      SelectionStatement
      "=>"
      SWITCH
      LPAREN
      "."
      Expression
      RPAREN
      Block)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s226))
    (STATE
     s226
     (COMMENT
      SelectionStatement
      "=>"
      SWITCH
      LPAREN
      Expression
      "."
      RPAREN
      Block)
     (SHIFT (RPAREN) s227))
    (STATE
     s227
     (COMMENT
      SelectionStatement
      "=>"
      SWITCH
      LPAREN
      Expression
      RPAREN
      "."
      Block)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (SHIFT (LCURLY) s86)
     (GOTO Block s228))
    (STATE
     s228
     (COMMENT
      SelectionStatement
      "=>"
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block
      ".")
     (REDUCE () r134))
    (STATE
     s229
     (COMMENT JumpStatement "=>" RETURN Expression "." SEMICOLON)
     (SHIFT (SEMICOLON) s231))
    (STATE
     s230
     (COMMENT JumpStatement "=>" RETURN SEMICOLON ".")
     (REDUCE () r152))
    (STATE
     s231
     (COMMENT JumpStatement "=>" RETURN Expression SEMICOLON ".")
     (REDUCE () r151))
    (STATE
     s232
     (COMMENT Dims "=>" "." Dims OP_DIM)
     (COMMENT Dims "=>" "." OP_DIM)
     (COMMENT DimExpr "=>" "." LBRACK Expression RBRACK)
     (COMMENT DimExprs "=>" "." DimExprs DimExpr)
     (COMMENT DimExprs "=>" "." DimExpr)
     (COMMENT ArrayAllocationExpression "=>" NEW TypeName "." Dims)
     (COMMENT ArrayAllocationExpression "=>" NEW TypeName "." DimExprs)
     (COMMENT ArrayAllocationExpression "=>" NEW TypeName "." DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" NEW TypeName "." LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      NEW
      TypeName
      "."
      LPAREN
      ArgumentList
      RPAREN)
     (SHIFT (OP_DIM) s193)
     (SHIFT (LPAREN) s236)
     (SHIFT (LBRACK) s237)
     (GOTO DimExprs s233)
     (GOTO DimExpr s234)
     (GOTO Dims s235))
    (STATE
     s233
     (COMMENT Dims "=>" "." Dims OP_DIM)
     (COMMENT Dims "=>" "." OP_DIM)
     (COMMENT DimExpr "=>" "." LBRACK Expression RBRACK)
     (COMMENT DimExprs "=>" DimExprs "." DimExpr)
     (COMMENT ArrayAllocationExpression "=>" NEW TypeName DimExprs ".")
     (COMMENT ArrayAllocationExpression "=>" NEW TypeName DimExprs "." Dims)
     (REDUCE (INSTANCEOF) r204)
     (REDUCE (OP_INC) r204)
     (REDUCE (OP_DEC) r204)
     (REDUCE (OP_SHL) r204)
     (REDUCE (OP_SHR) r204)
     (REDUCE (OP_SHRR) r204)
     (REDUCE (OP_GE) r204)
     (REDUCE (OP_LE) r204)
     (REDUCE (OP_EQ) r204)
     (REDUCE (OP_NE) r204)
     (REDUCE (OP_LAND) r204)
     (REDUCE (OP_LOR) r204)
     (SHIFT (OP_DIM) s193)
     (REDUCE (ASS_MUL) r204)
     (REDUCE (ASS_DIV) r204)
     (REDUCE (ASS_MOD) r204)
     (REDUCE (ASS_ADD) r204)
     (REDUCE (ASS_SUB) r204)
     (REDUCE (ASS_SHL) r204)
     (REDUCE (ASS_SHR) r204)
     (REDUCE (ASS_SHRR) r204)
     (REDUCE (ASS_AND) r204)
     (REDUCE (ASS_XOR) r204)
     (REDUCE (ASS_OR) r204)
     (REDUCE (SEMICOLON) r204)
     (REDUCE (COMMA) r204)
     (REDUCE (RPAREN) r204)
     (SHIFT (LBRACK) s237)
     (REDUCE (RBRACK) r204)
     (REDUCE (LCURLY) r204)
     (REDUCE (RCURLY) r204)
     (REDUCE (EQUAL_SIGN) r204)
     (REDUCE (DOT) r204)
     (REDUCE (PLUS) r204)
     (REDUCE (STAR) r204)
     (REDUCE (FORWARD_SLASH) r204)
     (REDUCE (MINUS) r204)
     (REDUCE (PERCENT) r204)
     (REDUCE (LESS_THAN) r204)
     (REDUCE (GREATER_THAN) r204)
     (REDUCE (COLON) r204)
     (REDUCE (PIPE) r204)
     (REDUCE (QMARK) r204)
     (REDUCE (AMPERSAND) r204)
     (REDUCE (CARROT) r204)
     (GOTO DimExpr s246)
     (GOTO Dims s247))
    (STATE s234 (COMMENT DimExprs "=>" DimExpr ".") (REDUCE () r206))
    (STATE
     s235
     (COMMENT Dims "=>" Dims "." OP_DIM)
     (COMMENT ArrayAllocationExpression "=>" NEW TypeName Dims ".")
     (REDUCE (INSTANCEOF) r205)
     (REDUCE (OP_INC) r205)
     (REDUCE (OP_DEC) r205)
     (REDUCE (OP_SHL) r205)
     (REDUCE (OP_SHR) r205)
     (REDUCE (OP_SHRR) r205)
     (REDUCE (OP_GE) r205)
     (REDUCE (OP_LE) r205)
     (REDUCE (OP_EQ) r205)
     (REDUCE (OP_NE) r205)
     (REDUCE (OP_LAND) r205)
     (REDUCE (OP_LOR) r205)
     (SHIFT (OP_DIM) s194)
     (REDUCE (ASS_MUL) r205)
     (REDUCE (ASS_DIV) r205)
     (REDUCE (ASS_MOD) r205)
     (REDUCE (ASS_ADD) r205)
     (REDUCE (ASS_SUB) r205)
     (REDUCE (ASS_SHL) r205)
     (REDUCE (ASS_SHR) r205)
     (REDUCE (ASS_SHRR) r205)
     (REDUCE (ASS_AND) r205)
     (REDUCE (ASS_XOR) r205)
     (REDUCE (ASS_OR) r205)
     (REDUCE (SEMICOLON) r205)
     (REDUCE (COMMA) r205)
     (REDUCE (RPAREN) r205)
     (REDUCE (RBRACK) r205)
     (REDUCE (LCURLY) r205)
     (REDUCE (RCURLY) r205)
     (REDUCE (EQUAL_SIGN) r205)
     (REDUCE (DOT) r205)
     (REDUCE (PLUS) r205)
     (REDUCE (STAR) r205)
     (REDUCE (FORWARD_SLASH) r205)
     (REDUCE (MINUS) r205)
     (REDUCE (PERCENT) r205)
     (REDUCE (LESS_THAN) r205)
     (REDUCE (GREATER_THAN) r205)
     (REDUCE (COLON) r205)
     (REDUCE (PIPE) r205)
     (REDUCE (QMARK) r205)
     (REDUCE (AMPERSAND) r205)
     (REDUCE (CARROT) r205))
    (STATE
     s236
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" NEW TypeName LPAREN "." RPAREN)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      NEW
      TypeName
      LPAREN
      "."
      ArgumentList
      RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT ArgumentList "=>" "." ArgumentList COMMA Expression)
     (COMMENT ArgumentList "=>" "." Expression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (RPAREN) s242)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO ArgumentList s240)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s241))
    (STATE
     s237
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT DimExpr "=>" LBRACK "." Expression RBRACK)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s238))
    (STATE
     s238
     (COMMENT DimExpr "=>" LBRACK Expression "." RBRACK)
     (SHIFT (RBRACK) s239))
    (STATE
     s239
     (COMMENT DimExpr "=>" LBRACK Expression RBRACK ".")
     (REDUCE () r208))
    (STATE
     s240
     (COMMENT
      ClassAllocationExpression
      "=>"
      NEW
      TypeName
      LPAREN
      ArgumentList
      "."
      RPAREN)
     (COMMENT ArgumentList "=>" ArgumentList "." COMMA Expression)
     (SHIFT (COMMA) s243)
     (SHIFT (RPAREN) s244))
    (STATE s241 (COMMENT ArgumentList "=>" Expression ".") (REDUCE () r191))
    (STATE
     s242
     (COMMENT ClassAllocationExpression "=>" NEW TypeName LPAREN RPAREN ".")
     (REDUCE () r202))
    (STATE
     s243
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT ArgumentList "=>" ArgumentList COMMA "." Expression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s245))
    (STATE
     s244
     (COMMENT
      ClassAllocationExpression
      "=>"
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN
      ".")
     (REDUCE () r201))
    (STATE
     s245
     (COMMENT ArgumentList "=>" ArgumentList COMMA Expression ".")
     (REDUCE () r192))
    (STATE s246 (COMMENT DimExprs "=>" DimExprs DimExpr ".") (REDUCE () r207))
    (STATE
     s247
     (COMMENT Dims "=>" Dims "." OP_DIM)
     (COMMENT ArrayAllocationExpression "=>" NEW TypeName DimExprs Dims ".")
     (REDUCE (INSTANCEOF) r203)
     (REDUCE (OP_INC) r203)
     (REDUCE (OP_DEC) r203)
     (REDUCE (OP_SHL) r203)
     (REDUCE (OP_SHR) r203)
     (REDUCE (OP_SHRR) r203)
     (REDUCE (OP_GE) r203)
     (REDUCE (OP_LE) r203)
     (REDUCE (OP_EQ) r203)
     (REDUCE (OP_NE) r203)
     (REDUCE (OP_LAND) r203)
     (REDUCE (OP_LOR) r203)
     (SHIFT (OP_DIM) s194)
     (REDUCE (ASS_MUL) r203)
     (REDUCE (ASS_DIV) r203)
     (REDUCE (ASS_MOD) r203)
     (REDUCE (ASS_ADD) r203)
     (REDUCE (ASS_SUB) r203)
     (REDUCE (ASS_SHL) r203)
     (REDUCE (ASS_SHR) r203)
     (REDUCE (ASS_SHRR) r203)
     (REDUCE (ASS_AND) r203)
     (REDUCE (ASS_XOR) r203)
     (REDUCE (ASS_OR) r203)
     (REDUCE (SEMICOLON) r203)
     (REDUCE (COMMA) r203)
     (REDUCE (RPAREN) r203)
     (REDUCE (RBRACK) r203)
     (REDUCE (LCURLY) r203)
     (REDUCE (RCURLY) r203)
     (REDUCE (EQUAL_SIGN) r203)
     (REDUCE (DOT) r203)
     (REDUCE (PLUS) r203)
     (REDUCE (STAR) r203)
     (REDUCE (FORWARD_SLASH) r203)
     (REDUCE (MINUS) r203)
     (REDUCE (PERCENT) r203)
     (REDUCE (LESS_THAN) r203)
     (REDUCE (GREATER_THAN) r203)
     (REDUCE (COLON) r203)
     (REDUCE (PIPE) r203)
     (REDUCE (QMARK) r203)
     (REDUCE (AMPERSAND) r203)
     (REDUCE (CARROT) r203))
    (STATE
     s248
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      "."
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      "."
      Expression
      RPAREN
      Statement)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s249))
    (STATE
     s249
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      Expression
      "."
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      Expression
      "."
      RPAREN
      Statement)
     (SHIFT (RPAREN) s250))
    (STATE
     s250
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches Finally)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches)
     (COMMENT GuardingStatement "=>" "." TRY Block Finally)
     (COMMENT
      GuardingStatement
      "=>"
      "."
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT JumpStatement "=>" "." THROW Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE IDENTIFIER SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK IDENTIFIER SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block)
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      Expression
      RPAREN
      "."
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      Expression
      RPAREN
      "."
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT LabelStatement "=>" "." DEFAULT COLON)
     (COMMENT LabelStatement "=>" "." CASE ConstantExpression COLON)
     (COMMENT LabelStatement "=>" "." IDENTIFIER COLON)
     (COMMENT EmptyStatement "=>" "." SEMICOLON)
     (COMMENT Statement "=>" "." Block)
     (COMMENT Statement "=>" "." GuardingStatement)
     (COMMENT Statement "=>" "." JumpStatement)
     (COMMENT Statement "=>" "." IterationStatement)
     (COMMENT Statement "=>" "." SelectionStatement)
     (COMMENT Statement "=>" "." ExpressionStatement SEMICOLON)
     (COMMENT Statement "=>" "." LabelStatement)
     (COMMENT Statement "=>" "." EmptyStatement)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BREAK) s136)
     (SHIFT (BYTE) s52)
     (SHIFT (CASE) s137)
     (SHIFT (CHAR) s53)
     (SHIFT (CONTINUE) s138)
     (SHIFT (DEFAULT) s139)
     (SHIFT (DO) s140)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (FOR) s142)
     (SHIFT (IF) s143)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (RETURN) s146)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (SWITCH) s148)
     (SHIFT (SYNCHRONIZED) s149)
     (SHIFT (THIS) s150)
     (SHIFT (THROW) s151)
     (SHIFT (TRY) s152)
     (SHIFT (VOID) s59)
     (SHIFT (WHILE) s153)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s156)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s159)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s86)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO Block s91)
     (GOTO Statement s251)
     (GOTO EmptyStatement s96)
     (GOTO LabelStatement s97)
     (GOTO ExpressionStatement s98)
     (GOTO SelectionStatement s99)
     (GOTO IterationStatement s100)
     (GOTO JumpStatement s101)
     (GOTO GuardingStatement s102)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s251
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      "."
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ".")
     (REDUCE (BOOLEAN) r132)
     (REDUCE (BREAK) r132)
     (REDUCE (BYTE) r132)
     (REDUCE (CASE) r132)
     (REDUCE (CHAR) r132)
     (REDUCE (CONTINUE) r132)
     (REDUCE (DEFAULT) r132)
     (REDUCE (DO) r132)
     (REDUCE (DOUBLE) r132)
     (COMMENT (REDUCE (ELSE) r132))
     (SHIFT (ELSE) s252)
     (REDUCE (FINAL) r132)
     (REDUCE (FLOAT) r132)
     (REDUCE (FOR) r132)
     (REDUCE (IF) r132)
     (REDUCE (INT) r132)
     (REDUCE (LONG) r132)
     (REDUCE (NEW) r132)
     (REDUCE (JNULL) r132)
     (REDUCE (RETURN) r132)
     (REDUCE (SHORT) r132)
     (REDUCE (SUPER) r132)
     (REDUCE (SWITCH) r132)
     (REDUCE (SYNCHRONIZED) r132)
     (REDUCE (THIS) r132)
     (REDUCE (THROW) r132)
     (REDUCE (TRY) r132)
     (REDUCE (VOID) r132)
     (REDUCE (WHILE) r132)
     (REDUCE (OP_INC) r132)
     (REDUCE (OP_DEC) r132)
     (REDUCE (IDENTIFIER) r132)
     (REDUCE (LITERAL) r132)
     (REDUCE (BOOLLIT) r132)
     (REDUCE (SEMICOLON) r132)
     (REDUCE (LPAREN) r132)
     (REDUCE (LCURLY) r132)
     (REDUCE (RCURLY) r132)
     (REDUCE (PLUS) r132)
     (REDUCE (MINUS) r132)
     (REDUCE (BANG) r132)
     (REDUCE (TWIDDLE) r132))
    (STATE
     s252
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches Finally)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches)
     (COMMENT GuardingStatement "=>" "." TRY Block Finally)
     (COMMENT
      GuardingStatement
      "=>"
      "."
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT JumpStatement "=>" "." THROW Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE IDENTIFIER SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK IDENTIFIER SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block)
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      "."
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT LabelStatement "=>" "." DEFAULT COLON)
     (COMMENT LabelStatement "=>" "." CASE ConstantExpression COLON)
     (COMMENT LabelStatement "=>" "." IDENTIFIER COLON)
     (COMMENT EmptyStatement "=>" "." SEMICOLON)
     (COMMENT Statement "=>" "." Block)
     (COMMENT Statement "=>" "." GuardingStatement)
     (COMMENT Statement "=>" "." JumpStatement)
     (COMMENT Statement "=>" "." IterationStatement)
     (COMMENT Statement "=>" "." SelectionStatement)
     (COMMENT Statement "=>" "." ExpressionStatement SEMICOLON)
     (COMMENT Statement "=>" "." LabelStatement)
     (COMMENT Statement "=>" "." EmptyStatement)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BREAK) s136)
     (SHIFT (BYTE) s52)
     (SHIFT (CASE) s137)
     (SHIFT (CHAR) s53)
     (SHIFT (CONTINUE) s138)
     (SHIFT (DEFAULT) s139)
     (SHIFT (DO) s140)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (FOR) s142)
     (SHIFT (IF) s143)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (RETURN) s146)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (SWITCH) s148)
     (SHIFT (SYNCHRONIZED) s149)
     (SHIFT (THIS) s150)
     (SHIFT (THROW) s151)
     (SHIFT (TRY) s152)
     (SHIFT (VOID) s59)
     (SHIFT (WHILE) s153)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s156)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s159)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s86)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO Block s91)
     (GOTO Statement s253)
     (GOTO EmptyStatement s96)
     (GOTO LabelStatement s97)
     (GOTO ExpressionStatement s98)
     (GOTO SelectionStatement s99)
     (GOTO IterationStatement s100)
     (GOTO JumpStatement s101)
     (GOTO GuardingStatement s102)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s253
     (COMMENT
      SelectionStatement
      "=>"
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement
      ".")
     (REDUCE () r133))
    (STATE
     s254
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT
      ExpressionStatements
      "=>"
      "."
      ExpressionStatements
      COMMA
      ExpressionStatement)
     (COMMENT ExpressionStatements "=>" "." ExpressionStatement)
     (COMMENT ForInit "=>" "." SEMICOLON)
     (COMMENT ForInit "=>" "." LocalVariableDeclarationStatement)
     (COMMENT ForInit "=>" "." ExpressionStatements SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      "."
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      "."
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      "."
      FINAL
      TypeSpecifier
      VariableDeclarators
      SEMICOLON)
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      "."
      TypeSpecifier
      VariableDeclarators
      SEMICOLON)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s141)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s259)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO TypeSpecifier s88)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s89)
     (GOTO QualifiedName s90)
     (GOTO LocalVariableDeclarationStatement s255)
     (GOTO ExpressionStatement s256)
     (GOTO ForInit s257)
     (GOTO ExpressionStatements s258)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s255
     (COMMENT ForInit "=>" LocalVariableDeclarationStatement ".")
     (REDUCE () r140))
    (STATE
     s256
     (COMMENT ExpressionStatements "=>" ExpressionStatement ".")
     (REDUCE () r145))
    (STATE
     s257
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT ForExpr "=>" "." SEMICOLON)
     (COMMENT ForExpr "=>" "." Expression SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      ForInit
      "."
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      ForInit
      "."
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s265)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO ForExpr s263)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s264))
    (STATE
     s258
     (COMMENT
      ExpressionStatements
      "=>"
      ExpressionStatements
      "."
      COMMA
      ExpressionStatement)
     (COMMENT ForInit "=>" ExpressionStatements "." SEMICOLON)
     (SHIFT (SEMICOLON) s260)
     (SHIFT (COMMA) s261))
    (STATE s259 (COMMENT ForInit "=>" SEMICOLON ".") (REDUCE () r141))
    (STATE
     s260
     (COMMENT ForInit "=>" ExpressionStatements SEMICOLON ".")
     (REDUCE () r139))
    (STATE
     s261
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT
      ExpressionStatements
      "=>"
      ExpressionStatements
      COMMA
      "."
      ExpressionStatement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO ExpressionStatement s262)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s262
     (COMMENT
      ExpressionStatements
      "=>"
      ExpressionStatements
      COMMA
      ExpressionStatement
      ".")
     (REDUCE () r146))
    (STATE
     s263
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT
      ExpressionStatements
      "=>"
      "."
      ExpressionStatements
      COMMA
      ExpressionStatement)
     (COMMENT ExpressionStatements "=>" "." ExpressionStatement)
     (COMMENT ForIncr "=>" "." ExpressionStatements)
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      ForInit
      ForExpr
      "."
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      ForInit
      ForExpr
      "."
      ForIncr
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (RPAREN) s269)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO ExpressionStatement s256)
     (GOTO ForIncr s267)
     (GOTO ExpressionStatements s268)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s264
     (COMMENT ForExpr "=>" Expression "." SEMICOLON)
     (SHIFT (SEMICOLON) s266))
    (STATE s265 (COMMENT ForExpr "=>" SEMICOLON ".") (REDUCE () r143))
    (STATE
     s266
     (COMMENT ForExpr "=>" Expression SEMICOLON ".")
     (REDUCE () r142))
    (STATE
     s267
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      "."
      RPAREN
      Statement)
     (SHIFT (RPAREN) s271))
    (STATE
     s268
     (COMMENT
      ExpressionStatements
      "=>"
      ExpressionStatements
      "."
      COMMA
      ExpressionStatement)
     (COMMENT ForIncr "=>" ExpressionStatements ".")
     (SHIFT (COMMA) s261)
     (REDUCE (RPAREN) r144))
    (STATE
     s269
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches Finally)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches)
     (COMMENT GuardingStatement "=>" "." TRY Block Finally)
     (COMMENT
      GuardingStatement
      "=>"
      "."
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT JumpStatement "=>" "." THROW Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE IDENTIFIER SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK IDENTIFIER SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      "."
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT LabelStatement "=>" "." DEFAULT COLON)
     (COMMENT LabelStatement "=>" "." CASE ConstantExpression COLON)
     (COMMENT LabelStatement "=>" "." IDENTIFIER COLON)
     (COMMENT EmptyStatement "=>" "." SEMICOLON)
     (COMMENT Statement "=>" "." Block)
     (COMMENT Statement "=>" "." GuardingStatement)
     (COMMENT Statement "=>" "." JumpStatement)
     (COMMENT Statement "=>" "." IterationStatement)
     (COMMENT Statement "=>" "." SelectionStatement)
     (COMMENT Statement "=>" "." ExpressionStatement SEMICOLON)
     (COMMENT Statement "=>" "." LabelStatement)
     (COMMENT Statement "=>" "." EmptyStatement)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BREAK) s136)
     (SHIFT (BYTE) s52)
     (SHIFT (CASE) s137)
     (SHIFT (CHAR) s53)
     (SHIFT (CONTINUE) s138)
     (SHIFT (DEFAULT) s139)
     (SHIFT (DO) s140)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (FOR) s142)
     (SHIFT (IF) s143)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (RETURN) s146)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (SWITCH) s148)
     (SHIFT (SYNCHRONIZED) s149)
     (SHIFT (THIS) s150)
     (SHIFT (THROW) s151)
     (SHIFT (TRY) s152)
     (SHIFT (VOID) s59)
     (SHIFT (WHILE) s153)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s156)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s159)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s86)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO Block s91)
     (GOTO Statement s270)
     (GOTO EmptyStatement s96)
     (GOTO LabelStatement s97)
     (GOTO ExpressionStatement s98)
     (GOTO SelectionStatement s99)
     (GOTO IterationStatement s100)
     (GOTO JumpStatement s101)
     (GOTO GuardingStatement s102)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s270
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement
      ".")
     (REDUCE () r138))
    (STATE
     s271
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches Finally)
     (COMMENT GuardingStatement "=>" "." TRY Block Catches)
     (COMMENT GuardingStatement "=>" "." TRY Block Finally)
     (COMMENT
      GuardingStatement
      "=>"
      "."
      SYNCHRONIZED
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT JumpStatement "=>" "." THROW Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN SEMICOLON)
     (COMMENT JumpStatement "=>" "." RETURN Expression SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE SEMICOLON)
     (COMMENT JumpStatement "=>" "." CONTINUE IDENTIFIER SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK SEMICOLON)
     (COMMENT JumpStatement "=>" "." BREAK IDENTIFIER SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      "."
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement)
     (COMMENT
      IterationStatement
      "=>"
      "."
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT
      IterationStatement
      "=>"
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      SWITCH
      LPAREN
      Expression
      RPAREN
      Block)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement
      ELSE
      Statement)
     (COMMENT
      SelectionStatement
      "=>"
      "."
      IF
      LPAREN
      Expression
      RPAREN
      Statement)
     (COMMENT ExpressionStatement "=>" "." Expression)
     (COMMENT LabelStatement "=>" "." DEFAULT COLON)
     (COMMENT LabelStatement "=>" "." CASE ConstantExpression COLON)
     (COMMENT LabelStatement "=>" "." IDENTIFIER COLON)
     (COMMENT EmptyStatement "=>" "." SEMICOLON)
     (COMMENT Statement "=>" "." Block)
     (COMMENT Statement "=>" "." GuardingStatement)
     (COMMENT Statement "=>" "." JumpStatement)
     (COMMENT Statement "=>" "." IterationStatement)
     (COMMENT Statement "=>" "." SelectionStatement)
     (COMMENT Statement "=>" "." ExpressionStatement SEMICOLON)
     (COMMENT Statement "=>" "." LabelStatement)
     (COMMENT Statement "=>" "." EmptyStatement)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BREAK) s136)
     (SHIFT (BYTE) s52)
     (SHIFT (CASE) s137)
     (SHIFT (CHAR) s53)
     (SHIFT (CONTINUE) s138)
     (SHIFT (DEFAULT) s139)
     (SHIFT (DO) s140)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (FOR) s142)
     (SHIFT (IF) s143)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (RETURN) s146)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (SWITCH) s148)
     (SHIFT (SYNCHRONIZED) s149)
     (SHIFT (THIS) s150)
     (SHIFT (THROW) s151)
     (SHIFT (TRY) s152)
     (SHIFT (VOID) s59)
     (SHIFT (WHILE) s153)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s156)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (SEMICOLON) s159)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s86)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO Block s91)
     (GOTO Statement s272)
     (GOTO EmptyStatement s96)
     (GOTO LabelStatement s97)
     (GOTO ExpressionStatement s98)
     (GOTO SelectionStatement s99)
     (GOTO IterationStatement s100)
     (GOTO JumpStatement s101)
     (GOTO GuardingStatement s102)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s135))
    (STATE
     s272
     (COMMENT
      IterationStatement
      "=>"
      FOR
      LPAREN
      ForInit
      ForExpr
      ForIncr
      RPAREN
      Statement
      ".")
     (REDUCE () r137))
    (STATE
     s273
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      FINAL
      TypeSpecifier
      "."
      VariableDeclarators
      SEMICOLON)
     (COMMENT DeclaratorName "=>" "." DeclaratorName OP_DIM)
     (COMMENT DeclaratorName "=>" "." IDENTIFIER)
     (COMMENT
      VariableDeclarator
      "=>"
      "."
      DeclaratorName
      EQUAL_SIGN
      VariableInitializer)
     (COMMENT VariableDeclarator "=>" "." DeclaratorName)
     (COMMENT
      VariableDeclarators
      "=>"
      "."
      VariableDeclarators
      COMMA
      VariableDeclarator)
     (COMMENT VariableDeclarators "=>" "." VariableDeclarator)
     (SHIFT (IDENTIFIER) s277)
     (GOTO VariableDeclarators s274)
     (GOTO VariableDeclarator s275)
     (GOTO DeclaratorName s276))
    (STATE
     s274
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      FINAL
      TypeSpecifier
      VariableDeclarators
      "."
      SEMICOLON)
     (COMMENT
      VariableDeclarators
      "=>"
      VariableDeclarators
      "."
      COMMA
      VariableDeclarator)
     (SHIFT (SEMICOLON) s289)
     (SHIFT (COMMA) s290))
    (STATE
     s275
     (COMMENT VariableDeclarators "=>" VariableDeclarator ".")
     (REDUCE () r75))
    (STATE
     s276
     (COMMENT DeclaratorName "=>" DeclaratorName "." OP_DIM)
     (COMMENT
      VariableDeclarator
      "=>"
      DeclaratorName
      "."
      EQUAL_SIGN
      VariableInitializer)
     (COMMENT VariableDeclarator "=>" DeclaratorName ".")
     (SHIFT (OP_DIM) s278)
     (REDUCE (SEMICOLON) r77)
     (REDUCE (COMMA) r77)
     (SHIFT (EQUAL_SIGN) s279))
    (STATE s277 (COMMENT DeclaratorName "=>" IDENTIFIER ".") (REDUCE () r96))
    (STATE
     s278
     (COMMENT DeclaratorName "=>" DeclaratorName OP_DIM ".")
     (REDUCE () r97))
    (STATE
     s279
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT VariableInitializer "=>" "." LCURLY ArrayInitializers RCURLY)
     (COMMENT VariableInitializer "=>" "." LCURLY RCURLY)
     (COMMENT VariableInitializer "=>" "." Expression)
     (COMMENT
      VariableDeclarator
      "=>"
      DeclaratorName
      EQUAL_SIGN
      "."
      VariableInitializer)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s282)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO VariableInitializer s280)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s281))
    (STATE
     s280
     (COMMENT
      VariableDeclarator
      "=>"
      DeclaratorName
      EQUAL_SIGN
      VariableInitializer
      ".")
     (REDUCE () r78))
    (STATE
     s281
     (COMMENT VariableInitializer "=>" Expression ".")
     (REDUCE () r79))
    (STATE
     s282
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT ArrayInitializers "=>" "." ArrayInitializers COMMA)
     (COMMENT
      ArrayInitializers
      "=>"
      "."
      ArrayInitializers
      COMMA
      VariableInitializer)
     (COMMENT ArrayInitializers "=>" "." VariableInitializer)
     (COMMENT VariableInitializer "=>" LCURLY "." ArrayInitializers RCURLY)
     (COMMENT VariableInitializer "=>" "." LCURLY ArrayInitializers RCURLY)
     (COMMENT VariableInitializer "=>" LCURLY "." RCURLY)
     (COMMENT VariableInitializer "=>" "." LCURLY RCURLY)
     (COMMENT VariableInitializer "=>" "." Expression)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s282)
     (SHIFT (RCURLY) s285)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO VariableInitializer s283)
     (GOTO ArrayInitializers s284)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s281))
    (STATE
     s283
     (COMMENT ArrayInitializers "=>" VariableInitializer ".")
     (REDUCE () r82))
    (STATE
     s284
     (COMMENT ArrayInitializers "=>" ArrayInitializers "." COMMA)
     (COMMENT
      ArrayInitializers
      "=>"
      ArrayInitializers
      "."
      COMMA
      VariableInitializer)
     (COMMENT VariableInitializer "=>" LCURLY ArrayInitializers "." RCURLY)
     (SHIFT (COMMA) s286)
     (SHIFT (RCURLY) s287))
    (STATE
     s285
     (COMMENT VariableInitializer "=>" LCURLY RCURLY ".")
     (REDUCE () r80))
    (STATE
     s286
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT ArrayInitializers "=>" ArrayInitializers COMMA ".")
     (COMMENT
      ArrayInitializers
      "=>"
      ArrayInitializers
      COMMA
      "."
      VariableInitializer)
     (COMMENT VariableInitializer "=>" "." LCURLY ArrayInitializers RCURLY)
     (COMMENT VariableInitializer "=>" "." LCURLY RCURLY)
     (COMMENT VariableInitializer "=>" "." Expression)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (REDUCE (COMMA) r84)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s282)
     (REDUCE (RCURLY) r84)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO VariableInitializer s288)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s281))
    (STATE
     s287
     (COMMENT VariableInitializer "=>" LCURLY ArrayInitializers RCURLY ".")
     (REDUCE () r81))
    (STATE
     s288
     (COMMENT
      ArrayInitializers
      "=>"
      ArrayInitializers
      COMMA
      VariableInitializer
      ".")
     (REDUCE () r83))
    (STATE
     s289
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      FINAL
      TypeSpecifier
      VariableDeclarators
      SEMICOLON
      ".")
     (REDUCE () r118))
    (STATE
     s290
     (COMMENT DeclaratorName "=>" "." DeclaratorName OP_DIM)
     (COMMENT DeclaratorName "=>" "." IDENTIFIER)
     (COMMENT
      VariableDeclarator
      "=>"
      "."
      DeclaratorName
      EQUAL_SIGN
      VariableInitializer)
     (COMMENT VariableDeclarator "=>" "." DeclaratorName)
     (COMMENT
      VariableDeclarators
      "=>"
      VariableDeclarators
      COMMA
      "."
      VariableDeclarator)
     (SHIFT (IDENTIFIER) s277)
     (GOTO VariableDeclarator s291)
     (GOTO DeclaratorName s276))
    (STATE
     s291
     (COMMENT
      VariableDeclarators
      "=>"
      VariableDeclarators
      COMMA
      VariableDeclarator
      ".")
     (REDUCE () r76))
    (STATE
     s292
     (COMMENT
      IterationStatement
      "=>"
      DO
      Statement
      "."
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (SHIFT (WHILE) s293))
    (STATE
     s293
     (COMMENT
      IterationStatement
      "=>"
      DO
      Statement
      WHILE
      "."
      LPAREN
      Expression
      RPAREN
      SEMICOLON)
     (SHIFT (LPAREN) s294))
    (STATE
     s294
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT
      IterationStatement
      "=>"
      DO
      Statement
      WHILE
      LPAREN
      "."
      Expression
      RPAREN
      SEMICOLON)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s295))
    (STATE
     s295
     (COMMENT
      IterationStatement
      "=>"
      DO
      Statement
      WHILE
      LPAREN
      Expression
      "."
      RPAREN
      SEMICOLON)
     (SHIFT (RPAREN) s296))
    (STATE
     s296
     (COMMENT
      IterationStatement
      "=>"
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      "."
      SEMICOLON)
     (SHIFT (SEMICOLON) s297))
    (STATE
     s297
     (COMMENT
      IterationStatement
      "=>"
      DO
      Statement
      WHILE
      LPAREN
      Expression
      RPAREN
      SEMICOLON
      ".")
     (REDUCE () r136))
    (STATE
     s298
     (COMMENT LabelStatement "=>" DEFAULT COLON ".")
     (REDUCE () r130))
    (STATE
     s299
     (COMMENT JumpStatement "=>" CONTINUE IDENTIFIER "." SEMICOLON)
     (SHIFT (SEMICOLON) s301))
    (STATE
     s300
     (COMMENT JumpStatement "=>" CONTINUE SEMICOLON ".")
     (REDUCE () r150))
    (STATE
     s301
     (COMMENT JumpStatement "=>" CONTINUE IDENTIFIER SEMICOLON ".")
     (REDUCE () r149))
    (STATE
     s302
     (COMMENT ConstantExpression "=>" ConditionalExpression ".")
     (REDUCE () r279))
    (STATE
     s303
     (COMMENT LabelStatement "=>" CASE ConstantExpression "." COLON)
     (SHIFT (COLON) s304))
    (STATE
     s304
     (COMMENT LabelStatement "=>" CASE ConstantExpression COLON ".")
     (REDUCE () r129))
    (STATE
     s305
     (COMMENT JumpStatement "=>" BREAK IDENTIFIER "." SEMICOLON)
     (SHIFT (SEMICOLON) s307))
    (STATE
     s306
     (COMMENT JumpStatement "=>" BREAK SEMICOLON ".")
     (REDUCE () r148))
    (STATE
     s307
     (COMMENT JumpStatement "=>" BREAK IDENTIFIER SEMICOLON ".")
     (REDUCE () r147))
    (STATE
     s308
     (COMMENT
      ConditionalOrExpression
      "=>"
      ConditionalOrExpression
      OP_LOR
      "."
      ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s313))
    (STATE
     s309
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      ConditionalOrExpression
      QMARK
      "."
      Expression
      COLON
      ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s310))
    (STATE
     s310
     (COMMENT
      ConditionalExpression
      "=>"
      ConditionalOrExpression
      QMARK
      Expression
      "."
      COLON
      ConditionalExpression)
     (SHIFT (COLON) s311))
    (STATE
     s311
     (COMMENT
      ConditionalExpression
      "=>"
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      "."
      ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s312))
    (STATE
     s312
     (COMMENT
      ConditionalExpression
      "=>"
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression
      ".")
     (REDUCE () r263))
    (STATE
     s313
     (COMMENT
      ConditionalOrExpression
      "=>"
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression
      ".")
     (COMMENT
      ConditionalAndExpression
      "=>"
      ConditionalAndExpression
      "."
      OP_LAND
      InclusiveOrExpression)
     (SHIFT (OP_LAND) s314)
     (REDUCE (OP_LOR) r261)
     (REDUCE (SEMICOLON) r261)
     (REDUCE (COMMA) r261)
     (REDUCE (RPAREN) r261)
     (REDUCE (RBRACK) r261)
     (REDUCE (RCURLY) r261)
     (REDUCE (COLON) r261)
     (REDUCE (QMARK) r261))
    (STATE
     s314
     (COMMENT
      ConditionalAndExpression
      "=>"
      ConditionalAndExpression
      OP_LAND
      "."
      InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s315))
    (STATE
     s315
     (COMMENT
      ConditionalAndExpression
      "=>"
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression
      ".")
     (COMMENT
      InclusiveOrExpression
      "=>"
      InclusiveOrExpression
      "."
      PIPE
      ExclusiveOrExpression)
     (REDUCE (OP_LAND) r259)
     (REDUCE (OP_LOR) r259)
     (REDUCE (SEMICOLON) r259)
     (REDUCE (COMMA) r259)
     (REDUCE (RPAREN) r259)
     (REDUCE (RBRACK) r259)
     (REDUCE (RCURLY) r259)
     (REDUCE (COLON) r259)
     (SHIFT (PIPE) s316)
     (REDUCE (QMARK) r259))
    (STATE
     s316
     (COMMENT
      InclusiveOrExpression
      "=>"
      InclusiveOrExpression
      PIPE
      "."
      ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s317))
    (STATE
     s317
     (COMMENT
      InclusiveOrExpression
      "=>"
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression
      ".")
     (COMMENT
      ExclusiveOrExpression
      "=>"
      ExclusiveOrExpression
      "."
      CARROT
      AndExpression)
     (REDUCE (OP_LAND) r257)
     (REDUCE (OP_LOR) r257)
     (REDUCE (SEMICOLON) r257)
     (REDUCE (COMMA) r257)
     (REDUCE (RPAREN) r257)
     (REDUCE (RBRACK) r257)
     (REDUCE (RCURLY) r257)
     (REDUCE (COLON) r257)
     (REDUCE (PIPE) r257)
     (REDUCE (QMARK) r257)
     (SHIFT (CARROT) s318))
    (STATE
     s318
     (COMMENT
      ExclusiveOrExpression
      "=>"
      ExclusiveOrExpression
      CARROT
      "."
      AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s319))
    (STATE
     s319
     (COMMENT
      ExclusiveOrExpression
      "=>"
      ExclusiveOrExpression
      CARROT
      AndExpression
      ".")
     (COMMENT
      AndExpression
      "=>"
      AndExpression
      "."
      AMPERSAND
      EqualityExpression)
     (REDUCE (OP_LAND) r255)
     (REDUCE (OP_LOR) r255)
     (REDUCE (SEMICOLON) r255)
     (REDUCE (COMMA) r255)
     (REDUCE (RPAREN) r255)
     (REDUCE (RBRACK) r255)
     (REDUCE (RCURLY) r255)
     (REDUCE (COLON) r255)
     (REDUCE (PIPE) r255)
     (REDUCE (QMARK) r255)
     (SHIFT (AMPERSAND) s320)
     (REDUCE (CARROT) r255))
    (STATE
     s320
     (COMMENT
      AndExpression
      "=>"
      AndExpression
      AMPERSAND
      "."
      EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s321))
    (STATE
     s321
     (COMMENT
      AndExpression
      "=>"
      AndExpression
      AMPERSAND
      EqualityExpression
      ".")
     (COMMENT
      EqualityExpression
      "=>"
      EqualityExpression
      "."
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      EqualityExpression
      "."
      OP_EQ
      RelationalExpression)
     (SHIFT (OP_EQ) s322)
     (SHIFT (OP_NE) s323)
     (REDUCE (OP_LAND) r253)
     (REDUCE (OP_LOR) r253)
     (REDUCE (SEMICOLON) r253)
     (REDUCE (COMMA) r253)
     (REDUCE (RPAREN) r253)
     (REDUCE (RBRACK) r253)
     (REDUCE (RCURLY) r253)
     (REDUCE (COLON) r253)
     (REDUCE (PIPE) r253)
     (REDUCE (QMARK) r253)
     (REDUCE (AMPERSAND) r253)
     (REDUCE (CARROT) r253))
    (STATE
     s322
     (COMMENT
      EqualityExpression
      "=>"
      EqualityExpression
      OP_EQ
      "."
      RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s351))
    (STATE
     s323
     (COMMENT
      EqualityExpression
      "=>"
      EqualityExpression
      OP_NE
      "."
      RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s324))
    (STATE
     s324
     (COMMENT
      EqualityExpression
      "=>"
      EqualityExpression
      OP_NE
      RelationalExpression
      ".")
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      LESS_THAN
      ShiftExpression)
     (SHIFT (INSTANCEOF) s325)
     (SHIFT (OP_GE) s326)
     (SHIFT (OP_LE) s327)
     (REDUCE (OP_EQ) r251)
     (REDUCE (OP_NE) r251)
     (REDUCE (OP_LAND) r251)
     (REDUCE (OP_LOR) r251)
     (REDUCE (SEMICOLON) r251)
     (REDUCE (COMMA) r251)
     (REDUCE (RPAREN) r251)
     (REDUCE (RBRACK) r251)
     (REDUCE (RCURLY) r251)
     (SHIFT (LESS_THAN) s328)
     (SHIFT (GREATER_THAN) s329)
     (REDUCE (COLON) r251)
     (REDUCE (PIPE) r251)
     (REDUCE (QMARK) r251)
     (REDUCE (AMPERSAND) r251)
     (REDUCE (CARROT) r251))
    (STATE
     s325
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      INSTANCEOF
      "."
      TypeSpecifier)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (GOTO TypeSpecifier s350)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50))
    (STATE
     s326
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      OP_GE
      "."
      ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s349))
    (STATE
     s327
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      OP_LE
      "."
      ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s348))
    (STATE
     s328
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      LESS_THAN
      "."
      ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s347))
    (STATE
     s329
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      GREATER_THAN
      "."
      ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s330))
    (STATE
     s330
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      GREATER_THAN
      ShiftExpression
      ".")
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHL
      AdditiveExpression)
     (REDUCE (INSTANCEOF) r245)
     (SHIFT (OP_SHL) s331)
     (SHIFT (OP_SHR) s332)
     (SHIFT (OP_SHRR) s333)
     (REDUCE (OP_GE) r245)
     (REDUCE (OP_LE) r245)
     (REDUCE (OP_EQ) r245)
     (REDUCE (OP_NE) r245)
     (REDUCE (OP_LAND) r245)
     (REDUCE (OP_LOR) r245)
     (REDUCE (SEMICOLON) r245)
     (REDUCE (COMMA) r245)
     (REDUCE (RPAREN) r245)
     (REDUCE (RBRACK) r245)
     (REDUCE (RCURLY) r245)
     (REDUCE (LESS_THAN) r245)
     (REDUCE (GREATER_THAN) r245)
     (REDUCE (COLON) r245)
     (REDUCE (PIPE) r245)
     (REDUCE (QMARK) r245)
     (REDUCE (AMPERSAND) r245)
     (REDUCE (CARROT) r245))
    (STATE
     s331
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      OP_SHL
      "."
      AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s346))
    (STATE
     s332
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      OP_SHR
      "."
      AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s345))
    (STATE
     s333
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      OP_SHRR
      "."
      AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s334))
    (STATE
     s334
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      OP_SHRR
      AdditiveExpression
      ".")
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      "."
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      "."
      PLUS
      MultiplicativeExpression)
     (REDUCE (INSTANCEOF) r242)
     (REDUCE (OP_SHL) r242)
     (REDUCE (OP_SHR) r242)
     (REDUCE (OP_SHRR) r242)
     (REDUCE (OP_GE) r242)
     (REDUCE (OP_LE) r242)
     (REDUCE (OP_EQ) r242)
     (REDUCE (OP_NE) r242)
     (REDUCE (OP_LAND) r242)
     (REDUCE (OP_LOR) r242)
     (REDUCE (SEMICOLON) r242)
     (REDUCE (COMMA) r242)
     (REDUCE (RPAREN) r242)
     (REDUCE (RBRACK) r242)
     (REDUCE (RCURLY) r242)
     (SHIFT (PLUS) s335)
     (SHIFT (MINUS) s336)
     (REDUCE (LESS_THAN) r242)
     (REDUCE (GREATER_THAN) r242)
     (REDUCE (COLON) r242)
     (REDUCE (PIPE) r242)
     (REDUCE (QMARK) r242)
     (REDUCE (AMPERSAND) r242)
     (REDUCE (CARROT) r242))
    (STATE
     s335
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      PLUS
      "."
      MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s344))
    (STATE
     s336
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      MINUS
      "."
      MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s337))
    (STATE
     s337
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      MINUS
      MultiplicativeExpression
      ".")
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      "."
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      "."
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      "."
      STAR
      CastExpression)
     (REDUCE (INSTANCEOF) r238)
     (REDUCE (OP_SHL) r238)
     (REDUCE (OP_SHR) r238)
     (REDUCE (OP_SHRR) r238)
     (REDUCE (OP_GE) r238)
     (REDUCE (OP_LE) r238)
     (REDUCE (OP_EQ) r238)
     (REDUCE (OP_NE) r238)
     (REDUCE (OP_LAND) r238)
     (REDUCE (OP_LOR) r238)
     (REDUCE (SEMICOLON) r238)
     (REDUCE (COMMA) r238)
     (REDUCE (RPAREN) r238)
     (REDUCE (RBRACK) r238)
     (REDUCE (RCURLY) r238)
     (REDUCE (PLUS) r238)
     (SHIFT (STAR) s338)
     (SHIFT (FORWARD_SLASH) s339)
     (REDUCE (MINUS) r238)
     (SHIFT (PERCENT) s340)
     (REDUCE (LESS_THAN) r238)
     (REDUCE (GREATER_THAN) r238)
     (REDUCE (COLON) r238)
     (REDUCE (PIPE) r238)
     (REDUCE (QMARK) r238)
     (REDUCE (AMPERSAND) r238)
     (REDUCE (CARROT) r238))
    (STATE
     s338
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      STAR
      "."
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s343))
    (STATE
     s339
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      FORWARD_SLASH
      "."
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s342))
    (STATE
     s340
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      PERCENT
      "."
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s188)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s341))
    (STATE
     s341
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      PERCENT
      CastExpression
      ".")
     (REDUCE () r235))
    (STATE
     s342
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression
      ".")
     (REDUCE () r234))
    (STATE
     s343
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      STAR
      CastExpression
      ".")
     (REDUCE () r233))
    (STATE
     s344
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      PLUS
      MultiplicativeExpression
      ".")
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      "."
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      "."
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      MultiplicativeExpression
      "."
      STAR
      CastExpression)
     (REDUCE (INSTANCEOF) r237)
     (REDUCE (OP_SHL) r237)
     (REDUCE (OP_SHR) r237)
     (REDUCE (OP_SHRR) r237)
     (REDUCE (OP_GE) r237)
     (REDUCE (OP_LE) r237)
     (REDUCE (OP_EQ) r237)
     (REDUCE (OP_NE) r237)
     (REDUCE (OP_LAND) r237)
     (REDUCE (OP_LOR) r237)
     (REDUCE (SEMICOLON) r237)
     (REDUCE (COMMA) r237)
     (REDUCE (RPAREN) r237)
     (REDUCE (RBRACK) r237)
     (REDUCE (RCURLY) r237)
     (REDUCE (PLUS) r237)
     (SHIFT (STAR) s338)
     (SHIFT (FORWARD_SLASH) s339)
     (REDUCE (MINUS) r237)
     (SHIFT (PERCENT) s340)
     (REDUCE (LESS_THAN) r237)
     (REDUCE (GREATER_THAN) r237)
     (REDUCE (COLON) r237)
     (REDUCE (PIPE) r237)
     (REDUCE (QMARK) r237)
     (REDUCE (AMPERSAND) r237)
     (REDUCE (CARROT) r237))
    (STATE
     s345
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      OP_SHR
      AdditiveExpression
      ".")
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      "."
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      "."
      PLUS
      MultiplicativeExpression)
     (REDUCE (INSTANCEOF) r241)
     (REDUCE (OP_SHL) r241)
     (REDUCE (OP_SHR) r241)
     (REDUCE (OP_SHRR) r241)
     (REDUCE (OP_GE) r241)
     (REDUCE (OP_LE) r241)
     (REDUCE (OP_EQ) r241)
     (REDUCE (OP_NE) r241)
     (REDUCE (OP_LAND) r241)
     (REDUCE (OP_LOR) r241)
     (REDUCE (SEMICOLON) r241)
     (REDUCE (COMMA) r241)
     (REDUCE (RPAREN) r241)
     (REDUCE (RBRACK) r241)
     (REDUCE (RCURLY) r241)
     (SHIFT (PLUS) s335)
     (SHIFT (MINUS) s336)
     (REDUCE (LESS_THAN) r241)
     (REDUCE (GREATER_THAN) r241)
     (REDUCE (COLON) r241)
     (REDUCE (PIPE) r241)
     (REDUCE (QMARK) r241)
     (REDUCE (AMPERSAND) r241)
     (REDUCE (CARROT) r241))
    (STATE
     s346
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      OP_SHL
      AdditiveExpression
      ".")
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      "."
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      AdditiveExpression
      "."
      PLUS
      MultiplicativeExpression)
     (REDUCE (INSTANCEOF) r240)
     (REDUCE (OP_SHL) r240)
     (REDUCE (OP_SHR) r240)
     (REDUCE (OP_SHRR) r240)
     (REDUCE (OP_GE) r240)
     (REDUCE (OP_LE) r240)
     (REDUCE (OP_EQ) r240)
     (REDUCE (OP_NE) r240)
     (REDUCE (OP_LAND) r240)
     (REDUCE (OP_LOR) r240)
     (REDUCE (SEMICOLON) r240)
     (REDUCE (COMMA) r240)
     (REDUCE (RPAREN) r240)
     (REDUCE (RBRACK) r240)
     (REDUCE (RCURLY) r240)
     (SHIFT (PLUS) s335)
     (SHIFT (MINUS) s336)
     (REDUCE (LESS_THAN) r240)
     (REDUCE (GREATER_THAN) r240)
     (REDUCE (COLON) r240)
     (REDUCE (PIPE) r240)
     (REDUCE (QMARK) r240)
     (REDUCE (AMPERSAND) r240)
     (REDUCE (CARROT) r240))
    (STATE
     s347
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      LESS_THAN
      ShiftExpression
      ".")
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHL
      AdditiveExpression)
     (REDUCE (INSTANCEOF) r244)
     (SHIFT (OP_SHL) s331)
     (SHIFT (OP_SHR) s332)
     (SHIFT (OP_SHRR) s333)
     (REDUCE (OP_GE) r244)
     (REDUCE (OP_LE) r244)
     (REDUCE (OP_EQ) r244)
     (REDUCE (OP_NE) r244)
     (REDUCE (OP_LAND) r244)
     (REDUCE (OP_LOR) r244)
     (REDUCE (SEMICOLON) r244)
     (REDUCE (COMMA) r244)
     (REDUCE (RPAREN) r244)
     (REDUCE (RBRACK) r244)
     (REDUCE (RCURLY) r244)
     (REDUCE (LESS_THAN) r244)
     (REDUCE (GREATER_THAN) r244)
     (REDUCE (COLON) r244)
     (REDUCE (PIPE) r244)
     (REDUCE (QMARK) r244)
     (REDUCE (AMPERSAND) r244)
     (REDUCE (CARROT) r244))
    (STATE
     s348
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      OP_LE
      ShiftExpression
      ".")
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHL
      AdditiveExpression)
     (REDUCE (INSTANCEOF) r246)
     (SHIFT (OP_SHL) s331)
     (SHIFT (OP_SHR) s332)
     (SHIFT (OP_SHRR) s333)
     (REDUCE (OP_GE) r246)
     (REDUCE (OP_LE) r246)
     (REDUCE (OP_EQ) r246)
     (REDUCE (OP_NE) r246)
     (REDUCE (OP_LAND) r246)
     (REDUCE (OP_LOR) r246)
     (REDUCE (SEMICOLON) r246)
     (REDUCE (COMMA) r246)
     (REDUCE (RPAREN) r246)
     (REDUCE (RBRACK) r246)
     (REDUCE (RCURLY) r246)
     (REDUCE (LESS_THAN) r246)
     (REDUCE (GREATER_THAN) r246)
     (REDUCE (COLON) r246)
     (REDUCE (PIPE) r246)
     (REDUCE (QMARK) r246)
     (REDUCE (AMPERSAND) r246)
     (REDUCE (CARROT) r246))
    (STATE
     s349
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      OP_GE
      ShiftExpression
      ".")
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      ShiftExpression
      "."
      OP_SHL
      AdditiveExpression)
     (REDUCE (INSTANCEOF) r247)
     (SHIFT (OP_SHL) s331)
     (SHIFT (OP_SHR) s332)
     (SHIFT (OP_SHRR) s333)
     (REDUCE (OP_GE) r247)
     (REDUCE (OP_LE) r247)
     (REDUCE (OP_EQ) r247)
     (REDUCE (OP_NE) r247)
     (REDUCE (OP_LAND) r247)
     (REDUCE (OP_LOR) r247)
     (REDUCE (SEMICOLON) r247)
     (REDUCE (COMMA) r247)
     (REDUCE (RPAREN) r247)
     (REDUCE (RBRACK) r247)
     (REDUCE (RCURLY) r247)
     (REDUCE (LESS_THAN) r247)
     (REDUCE (GREATER_THAN) r247)
     (REDUCE (COLON) r247)
     (REDUCE (PIPE) r247)
     (REDUCE (QMARK) r247)
     (REDUCE (AMPERSAND) r247)
     (REDUCE (CARROT) r247))
    (STATE
     s350
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      INSTANCEOF
      TypeSpecifier
      ".")
     (REDUCE () r248))
    (STATE
     s351
     (COMMENT
      EqualityExpression
      "=>"
      EqualityExpression
      OP_EQ
      RelationalExpression
      ".")
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      RelationalExpression
      "."
      LESS_THAN
      ShiftExpression)
     (SHIFT (INSTANCEOF) s325)
     (SHIFT (OP_GE) s326)
     (SHIFT (OP_LE) s327)
     (REDUCE (OP_EQ) r250)
     (REDUCE (OP_NE) r250)
     (REDUCE (OP_LAND) r250)
     (REDUCE (OP_LOR) r250)
     (REDUCE (SEMICOLON) r250)
     (REDUCE (COMMA) r250)
     (REDUCE (RPAREN) r250)
     (REDUCE (RBRACK) r250)
     (REDUCE (RCURLY) r250)
     (SHIFT (LESS_THAN) s328)
     (SHIFT (GREATER_THAN) s329)
     (REDUCE (COLON) r250)
     (REDUCE (PIPE) r250)
     (REDUCE (QMARK) r250)
     (REDUCE (AMPERSAND) r250)
     (REDUCE (CARROT) r250))
    (STATE
     s352
     (COMMENT UnaryExpression "=>" ArithmeticUnaryOperator CastExpression ".")
     (REDUCE () r217))
    (STATE
     s353
     (COMMENT
      LogicalUnaryExpression
      "=>"
      LogicalUnaryOperator
      UnaryExpression
      ".")
     (REDUCE () r220))
    (STATE
     s354
     (COMMENT
      AssignmentExpression
      "=>"
      UnaryExpression
      AssignmentOperator
      "."
      AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s367))
    (STATE s355 (COMMENT AssignmentOperator "=>" ASS_MUL ".") (REDUCE () r267))
    (STATE s356 (COMMENT AssignmentOperator "=>" ASS_DIV ".") (REDUCE () r268))
    (STATE s357 (COMMENT AssignmentOperator "=>" ASS_MOD ".") (REDUCE () r269))
    (STATE s358 (COMMENT AssignmentOperator "=>" ASS_ADD ".") (REDUCE () r270))
    (STATE s359 (COMMENT AssignmentOperator "=>" ASS_SUB ".") (REDUCE () r271))
    (STATE s360 (COMMENT AssignmentOperator "=>" ASS_SHL ".") (REDUCE () r272))
    (STATE s361 (COMMENT AssignmentOperator "=>" ASS_SHR ".") (REDUCE () r273))
    (STATE
     s362
     (COMMENT AssignmentOperator "=>" ASS_SHRR ".")
     (REDUCE () r274))
    (STATE s363 (COMMENT AssignmentOperator "=>" ASS_AND ".") (REDUCE () r275))
    (STATE s364 (COMMENT AssignmentOperator "=>" ASS_XOR ".") (REDUCE () r276))
    (STATE s365 (COMMENT AssignmentOperator "=>" ASS_OR ".") (REDUCE () r277))
    (STATE
     s366
     (COMMENT AssignmentOperator "=>" EQUAL_SIGN ".")
     (REDUCE () r266))
    (STATE
     s367
     (COMMENT
      AssignmentExpression
      "=>"
      UnaryExpression
      AssignmentOperator
      AssignmentExpression
      ".")
     (REDUCE () r265))
    (STATE
     s368
     (COMMENT FieldAccess "=>" RealPostfixExpression DOT "." IDENTIFIER)
     (SHIFT (IDENTIFIER) s369))
    (STATE
     s369
     (COMMENT FieldAccess "=>" RealPostfixExpression DOT IDENTIFIER ".")
     (REDUCE () r179))
    (STATE
     s370
     (COMMENT RealPostfixExpression "=>" PostfixExpression OP_INC ".")
     (REDUCE () r213))
    (STATE
     s371
     (COMMENT RealPostfixExpression "=>" PostfixExpression OP_DEC ".")
     (REDUCE () r214))
    (STATE
     s372
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ArrayAllocationExpression
      LCURLY
      "."
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ArrayAllocationExpression
      LCURLY
      "."
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT ArrayInitializers "=>" "." ArrayInitializers COMMA)
     (COMMENT
      ArrayInitializers
      "=>"
      "."
      ArrayInitializers
      COMMA
      VariableInitializer)
     (COMMENT ArrayInitializers "=>" "." VariableInitializer)
     (COMMENT VariableInitializer "=>" "." LCURLY ArrayInitializers RCURLY)
     (COMMENT VariableInitializer "=>" "." LCURLY RCURLY)
     (COMMENT VariableInitializer "=>" "." Expression)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (LCURLY) s282)
     (SHIFT (RCURLY) s374)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO VariableInitializer s283)
     (GOTO ArrayInitializers s373)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s281))
    (STATE
     s373
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      "."
      RCURLY)
     (COMMENT ArrayInitializers "=>" ArrayInitializers "." COMMA)
     (COMMENT
      ArrayInitializers
      "=>"
      ArrayInitializers
      "."
      COMMA
      VariableInitializer)
     (SHIFT (COMMA) s286)
     (SHIFT (RCURLY) s375))
    (STATE
     s374
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ArrayAllocationExpression
      LCURLY
      RCURLY
      ".")
     (REDUCE () r197))
    (STATE
     s375
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY
      ".")
     (REDUCE () r199))
    (STATE
     s376
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ClassAllocationExpression
      LCURLY
      "."
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ClassAllocationExpression
      LCURLY
      "."
      RCURLY)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT NonStaticInitializer "=>" "." Block)
     (COMMENT StaticInitializer "=>" "." STATIC Block)
     (COMMENT ConstructorDeclarator "=>" "." IDENTIFIER LPAREN RPAREN)
     (COMMENT
      ConstructorDeclarator
      "=>"
      "."
      IDENTIFIER
      LPAREN
      ParameterList
      RPAREN)
     (COMMENT ConstructorDeclaration "=>" "." ConstructorDeclarator Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      ConstructorDeclarator
      Throws
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      Modifiers
      ConstructorDeclarator
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      Modifiers
      ConstructorDeclarator
      Throws
      Block)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      TypeSpecifier
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      "."
      TypeSpecifier
      VariableDeclarators)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      VariableDeclarators)
     (COMMENT FieldDeclaration "=>" "." TypeDeclaration)
     (COMMENT FieldDeclaration "=>" "." NonStaticInitializer)
     (COMMENT FieldDeclaration "=>" "." StaticInitializer)
     (COMMENT FieldDeclaration "=>" "." ConstructorDeclaration)
     (COMMENT FieldDeclaration "=>" "." MethodDeclaration)
     (COMMENT FieldDeclaration "=>" "." FieldVariableDeclaration SEMICOLON)
     (COMMENT FieldDeclarationOptSemi "=>" "." FieldDeclaration SemiColons)
     (COMMENT FieldDeclarationOptSemi "=>" "." FieldDeclaration)
     (COMMENT
      FieldDeclarations
      "=>"
      "."
      FieldDeclarations
      FieldDeclarationOptSemi)
     (COMMENT FieldDeclarations "=>" "." FieldDeclarationOptSemi)
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (CLASS) s14)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s15)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (INTERFACE) s17)
     (SHIFT (LONG) s57)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (SHORT) s58)
     (SHIFT (STATIC) s84)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOID) s59)
     (SHIFT (VOLATILE) s26)
     (SHIFT (IDENTIFIER) s85)
     (SHIFT (LCURLY) s86)
     (SHIFT (RCURLY) s378)
     (GOTO TypeSpecifier s70)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50)
     (GOTO TypeDeclaration s72)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s73)
     (GOTO Modifier s11)
     (GOTO ClassWord s12)
     (GOTO FieldDeclarations s377)
     (GOTO FieldDeclarationOptSemi s75)
     (GOTO FieldDeclaration s76)
     (GOTO FieldVariableDeclaration s77)
     (GOTO MethodDeclaration s78)
     (GOTO ConstructorDeclaration s79)
     (GOTO ConstructorDeclarator s80)
     (GOTO StaticInitializer s81)
     (GOTO NonStaticInitializer s82)
     (GOTO Block s83))
    (STATE
     s377
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      "."
      RCURLY)
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT NonStaticInitializer "=>" "." Block)
     (COMMENT StaticInitializer "=>" "." STATIC Block)
     (COMMENT ConstructorDeclarator "=>" "." IDENTIFIER LPAREN RPAREN)
     (COMMENT
      ConstructorDeclarator
      "=>"
      "."
      IDENTIFIER
      LPAREN
      ParameterList
      RPAREN)
     (COMMENT ConstructorDeclaration "=>" "." ConstructorDeclarator Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      ConstructorDeclarator
      Throws
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      Modifiers
      ConstructorDeclarator
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      "."
      Modifiers
      ConstructorDeclarator
      Throws
      Block)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      TypeSpecifier
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      "."
      TypeSpecifier
      VariableDeclarators)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      "."
      Modifiers
      TypeSpecifier
      VariableDeclarators)
     (COMMENT FieldDeclaration "=>" "." TypeDeclaration)
     (COMMENT FieldDeclaration "=>" "." NonStaticInitializer)
     (COMMENT FieldDeclaration "=>" "." StaticInitializer)
     (COMMENT FieldDeclaration "=>" "." ConstructorDeclaration)
     (COMMENT FieldDeclaration "=>" "." MethodDeclaration)
     (COMMENT FieldDeclaration "=>" "." FieldVariableDeclaration SEMICOLON)
     (COMMENT FieldDeclarationOptSemi "=>" "." FieldDeclaration SemiColons)
     (COMMENT FieldDeclarationOptSemi "=>" "." FieldDeclaration)
     (COMMENT
      FieldDeclarations
      "=>"
      FieldDeclarations
      "."
      FieldDeclarationOptSemi)
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (CLASS) s14)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s15)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (INTERFACE) s17)
     (SHIFT (LONG) s57)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (SHORT) s58)
     (SHIFT (STATIC) s84)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOID) s59)
     (SHIFT (VOLATILE) s26)
     (SHIFT (IDENTIFIER) s85)
     (SHIFT (LCURLY) s86)
     (SHIFT (RCURLY) s380)
     (GOTO TypeSpecifier s70)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50)
     (GOTO TypeDeclaration s72)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s73)
     (GOTO Modifier s11)
     (GOTO ClassWord s12)
     (GOTO FieldDeclarationOptSemi s379)
     (GOTO FieldDeclaration s76)
     (GOTO FieldVariableDeclaration s77)
     (GOTO MethodDeclaration s78)
     (GOTO ConstructorDeclaration s79)
     (GOTO ConstructorDeclarator s80)
     (GOTO StaticInitializer s81)
     (GOTO NonStaticInitializer s82)
     (GOTO Block s83))
    (STATE
     s378
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ClassAllocationExpression
      LCURLY
      RCURLY
      ".")
     (REDUCE () r198))
    (STATE
     s379
     (COMMENT
      FieldDeclarations
      "=>"
      FieldDeclarations
      FieldDeclarationOptSemi
      ".")
     (REDUCE () r64))
    (STATE
     s380
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY
      ".")
     (REDUCE () r200))
    (STATE
     s381
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT ArgumentList "=>" "." ArgumentList COMMA Expression)
     (COMMENT ArgumentList "=>" "." Expression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" MethodAccess LPAREN "." RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" MethodAccess LPAREN "." ArgumentList RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (RPAREN) s383)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO ArgumentList s382)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s241))
    (STATE
     s382
     (COMMENT ArgumentList "=>" ArgumentList "." COMMA Expression)
     (COMMENT MethodCall "=>" MethodAccess LPAREN ArgumentList "." RPAREN)
     (SHIFT (COMMA) s243)
     (SHIFT (RPAREN) s384))
    (STATE
     s383
     (COMMENT MethodCall "=>" MethodAccess LPAREN RPAREN ".")
     (REDUCE () r184))
    (STATE
     s384
     (COMMENT MethodCall "=>" MethodAccess LPAREN ArgumentList RPAREN ".")
     (REDUCE () r183))
    (STATE
     s385
     (COMMENT Expression "=>" "." AssignmentExpression)
     (COMMENT
      AssignmentExpression
      "=>"
      "."
      UnaryExpression
      AssignmentOperator
      AssignmentExpression)
     (COMMENT AssignmentExpression "=>" "." ConditionalExpression)
     (COMMENT
      ConditionalExpression
      "=>"
      "."
      ConditionalOrExpression
      QMARK
      Expression
      COLON
      ConditionalExpression)
     (COMMENT ConditionalExpression "=>" "." ConditionalOrExpression)
     (COMMENT
      ConditionalOrExpression
      "=>"
      "."
      ConditionalOrExpression
      OP_LOR
      ConditionalAndExpression)
     (COMMENT ConditionalOrExpression "=>" "." ConditionalAndExpression)
     (COMMENT
      ConditionalAndExpression
      "=>"
      "."
      ConditionalAndExpression
      OP_LAND
      InclusiveOrExpression)
     (COMMENT ConditionalAndExpression "=>" "." InclusiveOrExpression)
     (COMMENT
      InclusiveOrExpression
      "=>"
      "."
      InclusiveOrExpression
      PIPE
      ExclusiveOrExpression)
     (COMMENT InclusiveOrExpression "=>" "." ExclusiveOrExpression)
     (COMMENT
      ExclusiveOrExpression
      "=>"
      "."
      ExclusiveOrExpression
      CARROT
      AndExpression)
     (COMMENT ExclusiveOrExpression "=>" "." AndExpression)
     (COMMENT
      AndExpression
      "=>"
      "."
      AndExpression
      AMPERSAND
      EqualityExpression)
     (COMMENT AndExpression "=>" "." EqualityExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_NE
      RelationalExpression)
     (COMMENT
      EqualityExpression
      "=>"
      "."
      EqualityExpression
      OP_EQ
      RelationalExpression)
     (COMMENT EqualityExpression "=>" "." RelationalExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      INSTANCEOF
      TypeSpecifier)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_GE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      OP_LE
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      GREATER_THAN
      ShiftExpression)
     (COMMENT
      RelationalExpression
      "=>"
      "."
      RelationalExpression
      LESS_THAN
      ShiftExpression)
     (COMMENT RelationalExpression "=>" "." ShiftExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHRR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHR
      AdditiveExpression)
     (COMMENT
      ShiftExpression
      "=>"
      "."
      ShiftExpression
      OP_SHL
      AdditiveExpression)
     (COMMENT ShiftExpression "=>" "." AdditiveExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      MINUS
      MultiplicativeExpression)
     (COMMENT
      AdditiveExpression
      "=>"
      "."
      AdditiveExpression
      PLUS
      MultiplicativeExpression)
     (COMMENT AdditiveExpression "=>" "." MultiplicativeExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      PERCENT
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      FORWARD_SLASH
      CastExpression)
     (COMMENT
      MultiplicativeExpression
      "=>"
      "."
      MultiplicativeExpression
      STAR
      CastExpression)
     (COMMENT MultiplicativeExpression "=>" "." CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      Expression
      RPAREN
      LogicalUnaryExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      ClassTypeExpression
      RPAREN
      CastExpression)
     (COMMENT
      CastExpression
      "=>"
      "."
      LPAREN
      PrimitiveTypeExpression
      RPAREN
      CastExpression)
     (COMMENT CastExpression "=>" "." UnaryExpression)
     (COMMENT ArithmeticUnaryOperator "=>" "." MINUS)
     (COMMENT ArithmeticUnaryOperator "=>" "." PLUS)
     (COMMENT LogicalUnaryOperator "=>" "." BANG)
     (COMMENT LogicalUnaryOperator "=>" "." TWIDDLE)
     (COMMENT
      LogicalUnaryExpression
      "=>"
      "."
      LogicalUnaryOperator
      UnaryExpression)
     (COMMENT LogicalUnaryExpression "=>" "." PostfixExpression)
     (COMMENT UnaryExpression "=>" "." LogicalUnaryExpression)
     (COMMENT UnaryExpression "=>" "." ArithmeticUnaryOperator CastExpression)
     (COMMENT UnaryExpression "=>" "." OP_DEC UnaryExpression)
     (COMMENT UnaryExpression "=>" "." OP_INC UnaryExpression)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_DEC)
     (COMMENT RealPostfixExpression "=>" "." PostfixExpression OP_INC)
     (COMMENT PostfixExpression "=>" "." RealPostfixExpression)
     (COMMENT PostfixExpression "=>" "." PrimaryExpression)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName Dims)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs)
     (COMMENT ArrayAllocationExpression "=>" "." NEW TypeName DimExprs Dims)
     (COMMENT ClassAllocationExpression "=>" "." NEW TypeName LPAREN RPAREN)
     (COMMENT
      ClassAllocationExpression
      "=>"
      "."
      NEW
      TypeName
      LPAREN
      ArgumentList
      RPAREN)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      ArrayInitializers
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ClassAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT
      PlainNewAllocationExpression
      "=>"
      "."
      ArrayAllocationExpression
      LCURLY
      RCURLY)
     (COMMENT PlainNewAllocationExpression "=>" "." ClassAllocationExpression)
     (COMMENT PlainNewAllocationExpression "=>" "." ArrayAllocationExpression)
     (COMMENT
      NewAllocationExpression
      "=>"
      "."
      QualifiedName
      DOT
      PlainNewAllocationExpression)
     (COMMENT NewAllocationExpression "=>" "." PlainNewAllocationExpression)
     (COMMENT SpecialName "=>" "." JNULL)
     (COMMENT SpecialName "=>" "." SUPER)
     (COMMENT SpecialName "=>" "." THIS)
     (COMMENT MethodAccess "=>" "." QualifiedName)
     (COMMENT MethodAccess "=>" "." SpecialName)
     (COMMENT MethodAccess "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN RPAREN)
     (COMMENT MethodCall "=>" "." MethodAccess LPAREN ArgumentList RPAREN)
     (COMMENT FieldAccess "=>" "." PrimitiveType DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT CLASS)
     (COMMENT FieldAccess "=>" "." QualifiedName DOT THIS)
     (COMMENT FieldAccess "=>" "." RealPostfixExpression DOT IDENTIFIER)
     (COMMENT FieldAccess "=>" "." NotJustName DOT IDENTIFIER)
     (COMMENT ArrayAccess "=>" ComplexPrimary LBRACK "." Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." ComplexPrimary LBRACK Expression RBRACK)
     (COMMENT ArrayAccess "=>" "." QualifiedName LBRACK Expression RBRACK)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." MethodCall)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." FieldAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." ArrayAccess)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." BOOLLIT)
     (COMMENT ComplexPrimaryNoParenthesis "=>" "." LITERAL)
     (COMMENT ComplexPrimary "=>" "." ComplexPrimaryNoParenthesis)
     (COMMENT ComplexPrimary "=>" "." LPAREN Expression RPAREN)
     (COMMENT NotJustName "=>" "." ComplexPrimary)
     (COMMENT NotJustName "=>" "." NewAllocationExpression)
     (COMMENT NotJustName "=>" "." SpecialName)
     (COMMENT PrimaryExpression "=>" "." NotJustName)
     (COMMENT PrimaryExpression "=>" "." QualifiedName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (NEW) s144)
     (SHIFT (JNULL) s145)
     (SHIFT (SHORT) s58)
     (SHIFT (SUPER) s147)
     (SHIFT (THIS) s150)
     (SHIFT (VOID) s59)
     (SHIFT (OP_INC) s154)
     (SHIFT (OP_DEC) s155)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (LITERAL) s157)
     (SHIFT (BOOLLIT) s158)
     (SHIFT (LPAREN) s160)
     (SHIFT (PLUS) s162)
     (SHIFT (MINUS) s163)
     (SHIFT (BANG) s164)
     (SHIFT (TWIDDLE) s165)
     (GOTO PrimitiveType s172)
     (GOTO QualifiedName s173)
     (GOTO PrimaryExpression s103)
     (GOTO NotJustName s104)
     (GOTO ComplexPrimary s105)
     (GOTO ComplexPrimaryNoParenthesis s106)
     (GOTO ArrayAccess s107)
     (GOTO FieldAccess s108)
     (GOTO MethodCall s109)
     (GOTO MethodAccess s110)
     (GOTO SpecialName s111)
     (GOTO NewAllocationExpression s112)
     (GOTO PlainNewAllocationExpression s113)
     (GOTO ClassAllocationExpression s114)
     (GOTO ArrayAllocationExpression s115)
     (GOTO PostfixExpression s116)
     (GOTO RealPostfixExpression s117)
     (GOTO UnaryExpression s118)
     (GOTO LogicalUnaryExpression s119)
     (GOTO LogicalUnaryOperator s120)
     (GOTO ArithmeticUnaryOperator s121)
     (GOTO CastExpression s122)
     (GOTO MultiplicativeExpression s123)
     (GOTO AdditiveExpression s124)
     (GOTO ShiftExpression s125)
     (GOTO RelationalExpression s126)
     (GOTO EqualityExpression s127)
     (GOTO AndExpression s128)
     (GOTO ExclusiveOrExpression s129)
     (GOTO InclusiveOrExpression s130)
     (GOTO ConditionalAndExpression s131)
     (GOTO ConditionalOrExpression s132)
     (GOTO ConditionalExpression s133)
     (GOTO AssignmentExpression s134)
     (GOTO Expression s386))
    (STATE
     s386
     (COMMENT ArrayAccess "=>" ComplexPrimary LBRACK Expression "." RBRACK)
     (SHIFT (RBRACK) s387))
    (STATE
     s387
     (COMMENT ArrayAccess "=>" ComplexPrimary LBRACK Expression RBRACK ".")
     (REDUCE () r177))
    (STATE
     s388
     (COMMENT FieldAccess "=>" NotJustName DOT "." IDENTIFIER)
     (SHIFT (IDENTIFIER) s389))
    (STATE
     s389
     (COMMENT FieldAccess "=>" NotJustName DOT IDENTIFIER ".")
     (REDUCE () r178))
    (STATE
     s390
     (COMMENT Statement "=>" ExpressionStatement SEMICOLON ".")
     (REDUCE () r121))
    (STATE
     s391
     (COMMENT
      LocalVariableDeclarationsAndStatements
      "=>"
      LocalVariableDeclarationsAndStatements
      LocalVariableDeclarationOrStatement
      ".")
     (REDUCE () r114))
    (STATE
     s392
     (COMMENT
      Block
      "=>"
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY
      ".")
     (REDUCE () r111))
    (STATE
     s393
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      TypeSpecifier
      VariableDeclarators
      "."
      SEMICOLON)
     (COMMENT
      VariableDeclarators
      "=>"
      VariableDeclarators
      "."
      COMMA
      VariableDeclarator)
     (SHIFT (SEMICOLON) s394)
     (SHIFT (COMMA) s290))
    (STATE
     s394
     (COMMENT
      LocalVariableDeclarationStatement
      "=>"
      TypeSpecifier
      VariableDeclarators
      SEMICOLON
      ".")
     (REDUCE () r117))
    (STATE
     s395
     (COMMENT ConstructorDeclarator "=>" IDENTIFIER LPAREN "." RPAREN)
     (COMMENT
      ConstructorDeclarator
      "=>"
      IDENTIFIER
      LPAREN
      "."
      ParameterList
      RPAREN)
     (COMMENT Parameter "=>" "." FINAL TypeSpecifier DeclaratorName)
     (COMMENT Parameter "=>" "." TypeSpecifier DeclaratorName)
     (COMMENT ParameterList "=>" "." ParameterList COMMA Parameter)
     (COMMENT ParameterList "=>" "." Parameter)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s399)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (RPAREN) s400)
     (GOTO TypeSpecifier s396)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50)
     (GOTO ParameterList s397)
     (GOTO Parameter s398))
    (STATE
     s396
     (COMMENT DeclaratorName "=>" "." DeclaratorName OP_DIM)
     (COMMENT DeclaratorName "=>" "." IDENTIFIER)
     (COMMENT Parameter "=>" TypeSpecifier "." DeclaratorName)
     (SHIFT (IDENTIFIER) s277)
     (GOTO DeclaratorName s406))
    (STATE
     s397
     (COMMENT
      ConstructorDeclarator
      "=>"
      IDENTIFIER
      LPAREN
      ParameterList
      "."
      RPAREN)
     (COMMENT ParameterList "=>" ParameterList "." COMMA Parameter)
     (SHIFT (COMMA) s403)
     (SHIFT (RPAREN) s404))
    (STATE s398 (COMMENT ParameterList "=>" Parameter ".") (REDUCE () r92))
    (STATE
     s399
     (COMMENT Parameter "=>" FINAL "." TypeSpecifier DeclaratorName)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (GOTO TypeSpecifier s401)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50))
    (STATE
     s400
     (COMMENT ConstructorDeclarator "=>" IDENTIFIER LPAREN RPAREN ".")
     (REDUCE () r106))
    (STATE
     s401
     (COMMENT DeclaratorName "=>" "." DeclaratorName OP_DIM)
     (COMMENT DeclaratorName "=>" "." IDENTIFIER)
     (COMMENT Parameter "=>" FINAL TypeSpecifier "." DeclaratorName)
     (SHIFT (IDENTIFIER) s277)
     (GOTO DeclaratorName s402))
    (STATE
     s402
     (COMMENT DeclaratorName "=>" DeclaratorName "." OP_DIM)
     (COMMENT Parameter "=>" FINAL TypeSpecifier DeclaratorName ".")
     (SHIFT (OP_DIM) s278)
     (REDUCE (COMMA) r95)
     (REDUCE (RPAREN) r95))
    (STATE
     s403
     (COMMENT Parameter "=>" "." FINAL TypeSpecifier DeclaratorName)
     (COMMENT Parameter "=>" "." TypeSpecifier DeclaratorName)
     (COMMENT ParameterList "=>" ParameterList COMMA "." Parameter)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s399)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (GOTO TypeSpecifier s396)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50)
     (GOTO Parameter s405))
    (STATE
     s404
     (COMMENT
      ConstructorDeclarator
      "=>"
      IDENTIFIER
      LPAREN
      ParameterList
      RPAREN
      ".")
     (REDUCE () r105))
    (STATE
     s405
     (COMMENT ParameterList "=>" ParameterList COMMA Parameter ".")
     (REDUCE () r93))
    (STATE
     s406
     (COMMENT DeclaratorName "=>" DeclaratorName "." OP_DIM)
     (COMMENT Parameter "=>" TypeSpecifier DeclaratorName ".")
     (SHIFT (OP_DIM) s278)
     (REDUCE (COMMA) r94)
     (REDUCE (RPAREN) r94))
    (STATE
     s407
     (COMMENT StaticInitializer "=>" STATIC Block ".")
     (REDUCE () r107))
    (STATE
     s408
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT
      ConstructorDeclaration
      "=>"
      ConstructorDeclarator
      Throws
      "."
      Block)
     (SHIFT (LCURLY) s86)
     (GOTO Block s412))
    (STATE
     s409
     (COMMENT ConstructorDeclaration "=>" ConstructorDeclarator Block ".")
     (REDUCE () r104))
    (STATE
     s410
     (COMMENT Throws "=>" THROWS "." ClassNameList)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT ClassNameList "=>" "." ClassNameList COMMA QualifiedName)
     (COMMENT ClassNameList "=>" "." QualifiedName)
     (SHIFT (IDENTIFIER) s28)
     (GOTO ClassNameList s411)
     (GOTO QualifiedName s45))
    (STATE
     s411
     (COMMENT Throws "=>" THROWS ClassNameList ".")
     (COMMENT ClassNameList "=>" ClassNameList "." COMMA QualifiedName)
     (REDUCE (SEMICOLON) r98)
     (SHIFT (COMMA) s46)
     (REDUCE (LCURLY) r98))
    (STATE
     s412
     (COMMENT
      ConstructorDeclaration
      "=>"
      ConstructorDeclarator
      Throws
      Block
      ".")
     (REDUCE () r103))
    (STATE
     s413
     (COMMENT FieldDeclaration "=>" FieldVariableDeclaration SEMICOLON ".")
     (REDUCE () r67))
    (STATE
     s414
     (COMMENT FieldDeclarationOptSemi "=>" FieldDeclaration SemiColons ".")
     (COMMENT SemiColons "=>" SemiColons "." SEMICOLON)
     (REDUCE (ABSTRACT) r66)
     (REDUCE (BOOLEAN) r66)
     (REDUCE (BYTE) r66)
     (REDUCE (CHAR) r66)
     (REDUCE (CLASS) r66)
     (REDUCE (DOUBLE) r66)
     (REDUCE (FINAL) r66)
     (REDUCE (FLOAT) r66)
     (REDUCE (INT) r66)
     (REDUCE (INTERFACE) r66)
     (REDUCE (LONG) r66)
     (REDUCE (NATIVE) r66)
     (REDUCE (PRIVATE) r66)
     (REDUCE (PROTECTED) r66)
     (REDUCE (PUBLIC) r66)
     (REDUCE (SHORT) r66)
     (REDUCE (STATIC) r66)
     (REDUCE (SYNCHRONIZED) r66)
     (REDUCE (TRANSIENT) r66)
     (REDUCE (VOID) r66)
     (REDUCE (VOLATILE) r66)
     (REDUCE (IDENTIFIER) r66)
     (SHIFT (SEMICOLON) s33)
     (REDUCE (LCURLY) r66)
     (REDUCE (RCURLY) r66))
    (STATE
     s415
     (COMMENT
      TypeDeclaration
      "=>"
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY
      ".")
     (REDUCE () r38))
    (STATE
     s416
     (COMMENT DeclaratorName "=>" "." DeclaratorName OP_DIM)
     (COMMENT DeclaratorName "=>" "." IDENTIFIER)
     (COMMENT MethodDeclarator "=>" "." MethodDeclarator OP_DIM)
     (COMMENT MethodDeclarator "=>" "." DeclaratorName LPAREN RPAREN)
     (COMMENT
      MethodDeclarator
      "=>"
      "."
      DeclaratorName
      LPAREN
      ParameterList
      RPAREN)
     (COMMENT
      MethodDeclaration
      "=>"
      Modifiers
      TypeSpecifier
      "."
      MethodDeclarator
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      Modifiers
      TypeSpecifier
      "."
      MethodDeclarator
      Throws
      MethodBody)
     (COMMENT
      VariableDeclarator
      "=>"
      "."
      DeclaratorName
      EQUAL_SIGN
      VariableInitializer)
     (COMMENT VariableDeclarator "=>" "." DeclaratorName)
     (COMMENT
      VariableDeclarators
      "=>"
      "."
      VariableDeclarators
      COMMA
      VariableDeclarator)
     (COMMENT VariableDeclarators "=>" "." VariableDeclarator)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      Modifiers
      TypeSpecifier
      "."
      VariableDeclarators)
     (SHIFT (IDENTIFIER) s277)
     (GOTO VariableDeclarators s421)
     (GOTO VariableDeclarator s275)
     (GOTO MethodDeclarator s422)
     (GOTO DeclaratorName s423))
    (STATE
     s417
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT
      ConstructorDeclaration
      "=>"
      Modifiers
      ConstructorDeclarator
      "."
      Block)
     (COMMENT
      ConstructorDeclaration
      "=>"
      Modifiers
      ConstructorDeclarator
      "."
      Throws
      Block)
     (COMMENT Throws "=>" "." THROWS ClassNameList)
     (SHIFT (THROWS) s410)
     (SHIFT (LCURLY) s86)
     (GOTO Throws s418)
     (GOTO Block s419))
    (STATE
     s418
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT
      ConstructorDeclaration
      "=>"
      Modifiers
      ConstructorDeclarator
      Throws
      "."
      Block)
     (SHIFT (LCURLY) s86)
     (GOTO Block s420))
    (STATE
     s419
     (COMMENT
      ConstructorDeclaration
      "=>"
      Modifiers
      ConstructorDeclarator
      Block
      ".")
     (REDUCE () r102))
    (STATE
     s420
     (COMMENT
      ConstructorDeclaration
      "=>"
      Modifiers
      ConstructorDeclarator
      Throws
      Block
      ".")
     (REDUCE () r101))
    (STATE
     s421
     (COMMENT
      VariableDeclarators
      "=>"
      VariableDeclarators
      "."
      COMMA
      VariableDeclarator)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      Modifiers
      TypeSpecifier
      VariableDeclarators
      ".")
     (REDUCE (SEMICOLON) r73)
     (SHIFT (COMMA) s290))
    (STATE
     s422
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT MethodBody "=>" "." SEMICOLON)
     (COMMENT MethodBody "=>" "." Block)
     (COMMENT Throws "=>" "." THROWS ClassNameList)
     (COMMENT MethodDeclarator "=>" MethodDeclarator "." OP_DIM)
     (COMMENT
      MethodDeclaration
      "=>"
      Modifiers
      TypeSpecifier
      MethodDeclarator
      "."
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      Modifiers
      TypeSpecifier
      MethodDeclarator
      "."
      Throws
      MethodBody)
     (SHIFT (THROWS) s410)
     (SHIFT (OP_DIM) s431)
     (SHIFT (SEMICOLON) s432)
     (SHIFT (LCURLY) s86)
     (GOTO Throws s428)
     (GOTO MethodBody s429)
     (GOTO Block s430))
    (STATE
     s423
     (COMMENT DeclaratorName "=>" DeclaratorName "." OP_DIM)
     (COMMENT MethodDeclarator "=>" DeclaratorName "." LPAREN RPAREN)
     (COMMENT
      MethodDeclarator
      "=>"
      DeclaratorName
      "."
      LPAREN
      ParameterList
      RPAREN)
     (COMMENT
      VariableDeclarator
      "=>"
      DeclaratorName
      "."
      EQUAL_SIGN
      VariableInitializer)
     (COMMENT VariableDeclarator "=>" DeclaratorName ".")
     (SHIFT (OP_DIM) s278)
     (REDUCE (SEMICOLON) r77)
     (REDUCE (COMMA) r77)
     (SHIFT (LPAREN) s424)
     (SHIFT (EQUAL_SIGN) s279))
    (STATE
     s424
     (COMMENT Parameter "=>" "." FINAL TypeSpecifier DeclaratorName)
     (COMMENT Parameter "=>" "." TypeSpecifier DeclaratorName)
     (COMMENT ParameterList "=>" "." ParameterList COMMA Parameter)
     (COMMENT ParameterList "=>" "." Parameter)
     (COMMENT MethodDeclarator "=>" DeclaratorName LPAREN "." RPAREN)
     (COMMENT
      MethodDeclarator
      "=>"
      DeclaratorName
      LPAREN
      "."
      ParameterList
      RPAREN)
     (COMMENT QualifiedName "=>" "." QualifiedName DOT IDENTIFIER)
     (COMMENT QualifiedName "=>" "." IDENTIFIER)
     (COMMENT PrimitiveType "=>" "." VOID)
     (COMMENT PrimitiveType "=>" "." DOUBLE)
     (COMMENT PrimitiveType "=>" "." FLOAT)
     (COMMENT PrimitiveType "=>" "." LONG)
     (COMMENT PrimitiveType "=>" "." INT)
     (COMMENT PrimitiveType "=>" "." SHORT)
     (COMMENT PrimitiveType "=>" "." BYTE)
     (COMMENT PrimitiveType "=>" "." CHAR)
     (COMMENT PrimitiveType "=>" "." BOOLEAN)
     (COMMENT TypeName "=>" "." QualifiedName)
     (COMMENT TypeName "=>" "." PrimitiveType)
     (COMMENT TypeSpecifier "=>" "." TypeName Dims)
     (COMMENT TypeSpecifier "=>" "." TypeName)
     (SHIFT (BOOLEAN) s51)
     (SHIFT (BYTE) s52)
     (SHIFT (CHAR) s53)
     (SHIFT (DOUBLE) s54)
     (SHIFT (FINAL) s399)
     (SHIFT (FLOAT) s55)
     (SHIFT (INT) s56)
     (SHIFT (LONG) s57)
     (SHIFT (SHORT) s58)
     (SHIFT (VOID) s59)
     (SHIFT (IDENTIFIER) s28)
     (SHIFT (RPAREN) s426)
     (GOTO TypeSpecifier s396)
     (GOTO TypeName s71)
     (GOTO PrimitiveType s49)
     (GOTO QualifiedName s50)
     (GOTO ParameterList s425)
     (GOTO Parameter s398))
    (STATE
     s425
     (COMMENT ParameterList "=>" ParameterList "." COMMA Parameter)
     (COMMENT
      MethodDeclarator
      "=>"
      DeclaratorName
      LPAREN
      ParameterList
      "."
      RPAREN)
     (SHIFT (COMMA) s403)
     (SHIFT (RPAREN) s427))
    (STATE
     s426
     (COMMENT MethodDeclarator "=>" DeclaratorName LPAREN RPAREN ".")
     (REDUCE () r90))
    (STATE
     s427
     (COMMENT
      MethodDeclarator
      "=>"
      DeclaratorName
      LPAREN
      ParameterList
      RPAREN
      ".")
     (REDUCE () r89))
    (STATE
     s428
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT MethodBody "=>" "." SEMICOLON)
     (COMMENT MethodBody "=>" "." Block)
     (COMMENT
      MethodDeclaration
      "=>"
      Modifiers
      TypeSpecifier
      MethodDeclarator
      Throws
      "."
      MethodBody)
     (SHIFT (SEMICOLON) s432)
     (SHIFT (LCURLY) s86)
     (GOTO MethodBody s433)
     (GOTO Block s430))
    (STATE
     s429
     (COMMENT
      MethodDeclaration
      "=>"
      Modifiers
      TypeSpecifier
      MethodDeclarator
      MethodBody
      ".")
     (REDUCE () r86))
    (STATE s430 (COMMENT MethodBody "=>" Block ".") (REDUCE () r99))
    (STATE
     s431
     (COMMENT MethodDeclarator "=>" MethodDeclarator OP_DIM ".")
     (REDUCE () r91))
    (STATE s432 (COMMENT MethodBody "=>" SEMICOLON ".") (REDUCE () r100))
    (STATE
     s433
     (COMMENT
      MethodDeclaration
      "=>"
      Modifiers
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody
      ".")
     (REDUCE () r85))
    (STATE
     s434
     (COMMENT Dims "=>" Dims "." OP_DIM)
     (COMMENT TypeSpecifier "=>" TypeName Dims ".")
     (REDUCE (INSTANCEOF) r3)
     (REDUCE (OP_GE) r3)
     (REDUCE (OP_LE) r3)
     (REDUCE (OP_EQ) r3)
     (REDUCE (OP_NE) r3)
     (REDUCE (OP_LAND) r3)
     (REDUCE (OP_LOR) r3)
     (SHIFT (OP_DIM) s194)
     (REDUCE (IDENTIFIER) r3)
     (REDUCE (SEMICOLON) r3)
     (REDUCE (COMMA) r3)
     (REDUCE (RPAREN) r3)
     (REDUCE (RBRACK) r3)
     (REDUCE (RCURLY) r3)
     (REDUCE (LESS_THAN) r3)
     (REDUCE (GREATER_THAN) r3)
     (REDUCE (COLON) r3)
     (REDUCE (PIPE) r3)
     (REDUCE (QMARK) r3)
     (REDUCE (AMPERSAND) r3)
     (REDUCE (CARROT) r3))
    (STATE
     s435
     (COMMENT
      VariableDeclarators
      "=>"
      VariableDeclarators
      "."
      COMMA
      VariableDeclarator)
     (COMMENT
      FieldVariableDeclaration
      "=>"
      TypeSpecifier
      VariableDeclarators
      ".")
     (REDUCE (SEMICOLON) r74)
     (SHIFT (COMMA) s290))
    (STATE
     s436
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT MethodBody "=>" "." SEMICOLON)
     (COMMENT MethodBody "=>" "." Block)
     (COMMENT Throws "=>" "." THROWS ClassNameList)
     (COMMENT MethodDeclarator "=>" MethodDeclarator "." OP_DIM)
     (COMMENT
      MethodDeclaration
      "=>"
      TypeSpecifier
      MethodDeclarator
      "."
      MethodBody)
     (COMMENT
      MethodDeclaration
      "=>"
      TypeSpecifier
      MethodDeclarator
      "."
      Throws
      MethodBody)
     (SHIFT (THROWS) s410)
     (SHIFT (OP_DIM) s431)
     (SHIFT (SEMICOLON) s432)
     (SHIFT (LCURLY) s86)
     (GOTO Throws s437)
     (GOTO MethodBody s438)
     (GOTO Block s430))
    (STATE
     s437
     (COMMENT Block "=>" "." LCURLY RCURLY)
     (COMMENT
      Block
      "=>"
      "."
      LCURLY
      LocalVariableDeclarationsAndStatements
      RCURLY)
     (COMMENT MethodBody "=>" "." SEMICOLON)
     (COMMENT MethodBody "=>" "." Block)
     (COMMENT
      MethodDeclaration
      "=>"
      TypeSpecifier
      MethodDeclarator
      Throws
      "."
      MethodBody)
     (SHIFT (SEMICOLON) s432)
     (SHIFT (LCURLY) s86)
     (GOTO MethodBody s439)
     (GOTO Block s430))
    (STATE
     s438
     (COMMENT
      MethodDeclaration
      "=>"
      TypeSpecifier
      MethodDeclarator
      MethodBody
      ".")
     (REDUCE () r88))
    (STATE
     s439
     (COMMENT
      MethodDeclaration
      "=>"
      TypeSpecifier
      MethodDeclarator
      Throws
      MethodBody
      ".")
     (REDUCE () r87))
    (STATE
     s440
     (COMMENT TypeDeclarationOptSemi "=>" TypeDeclaration SemiColons ".")
     (COMMENT SemiColons "=>" SemiColons "." SEMICOLON)
     (REDUCE (ABSTRACT) r31)
     (REDUCE (CLASS) r31)
     (REDUCE (FINAL) r31)
     (REDUCE (INTERFACE) r31)
     (REDUCE (NATIVE) r31)
     (REDUCE (PRIVATE) r31)
     (REDUCE (PROTECTED) r31)
     (REDUCE (PUBLIC) r31)
     (REDUCE (STATIC) r31)
     (REDUCE (SYNCHRONIZED) r31)
     (REDUCE (TRANSIENT) r31)
     (REDUCE (VOLATILE) r31)
     (SHIFT (SEMICOLON) s33)
     (REDUCE (*EOF*) r31))
    (STATE
     s441
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration SemiColons)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration)
     (COMMENT
      TypeDeclarations
      "=>"
      TypeDeclarations
      "."
      TypeDeclarationOptSemi)
     (COMMENT ProgramFile "=>" ImportStatements TypeDeclarations ".")
     (SHIFT (ABSTRACT) s13)
     (SHIFT (CLASS) s14)
     (SHIFT (FINAL) s15)
     (SHIFT (INTERFACE) s17)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOLATILE) s26)
     (REDUCE (*EOF*) r23)
     (GOTO TypeDeclarationOptSemi s443)
     (GOTO TypeDeclaration s8)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s10)
     (GOTO Modifier s11)
     (GOTO ClassWord s12))
    (STATE
     s442
     (COMMENT ImportStatements "=>" ImportStatements ImportStatement ".")
     (REDUCE () r33))
    (STATE
     s443
     (COMMENT
      TypeDeclarations
      "=>"
      TypeDeclarations
      TypeDeclarationOptSemi
      ".")
     (REDUCE () r29))
    (STATE
     s444
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration SemiColons)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration)
     (COMMENT
      TypeDeclarations
      "=>"
      TypeDeclarations
      "."
      TypeDeclarationOptSemi)
     (COMMENT ProgramFile "=>" PackageStatement TypeDeclarations ".")
     (SHIFT (ABSTRACT) s13)
     (SHIFT (CLASS) s14)
     (SHIFT (FINAL) s15)
     (SHIFT (INTERFACE) s17)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOLATILE) s26)
     (REDUCE (*EOF*) r22)
     (GOTO TypeDeclarationOptSemi s443)
     (GOTO TypeDeclaration s8)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s10)
     (GOTO Modifier s11)
     (GOTO ClassWord s12))
    (STATE
     s445
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT
      ImportStatement
      "=>"
      "."
      IMPORT
      QualifiedName
      DOT
      STAR
      SemiColons)
     (COMMENT ImportStatement "=>" "." IMPORT QualifiedName SemiColons)
     (COMMENT ImportStatements "=>" ImportStatements "." ImportStatement)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration SemiColons)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration)
     (COMMENT
      TypeDeclarations
      "=>"
      "."
      TypeDeclarations
      TypeDeclarationOptSemi)
     (COMMENT TypeDeclarations "=>" "." TypeDeclarationOptSemi)
     (COMMENT ProgramFile "=>" PackageStatement ImportStatements ".")
     (COMMENT
      ProgramFile
      "=>"
      PackageStatement
      ImportStatements
      "."
      TypeDeclarations)
     (SHIFT (ABSTRACT) s13)
     (SHIFT (CLASS) s14)
     (SHIFT (FINAL) s15)
     (SHIFT (IMPORT) s16)
     (SHIFT (INTERFACE) s17)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOLATILE) s26)
     (REDUCE (*EOF*) r21)
     (GOTO TypeDeclarations s446)
     (GOTO TypeDeclarationOptSemi s5)
     (GOTO ImportStatement s442)
     (GOTO TypeDeclaration s8)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s10)
     (GOTO Modifier s11)
     (GOTO ClassWord s12))
    (STATE
     s446
     (COMMENT ClassWord "=>" "." INTERFACE)
     (COMMENT ClassWord "=>" "." CLASS)
     (COMMENT Modifier "=>" "." SYNCHRONIZED)
     (COMMENT Modifier "=>" "." NATIVE)
     (COMMENT Modifier "=>" "." VOLATILE)
     (COMMENT Modifier "=>" "." TRANSIENT)
     (COMMENT Modifier "=>" "." STATIC)
     (COMMENT Modifier "=>" "." PRIVATE)
     (COMMENT Modifier "=>" "." PROTECTED)
     (COMMENT Modifier "=>" "." PUBLIC)
     (COMMENT Modifier "=>" "." FINAL)
     (COMMENT Modifier "=>" "." ABSTRACT)
     (COMMENT Modifiers "=>" "." Modifiers Modifier)
     (COMMENT Modifiers "=>" "." Modifier)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER)
     (COMMENT ClassHeader "=>" "." ClassWord IDENTIFIER Extends Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Interfaces)
     (COMMENT ClassHeader "=>" "." Modifiers ClassWord IDENTIFIER Extends)
     (COMMENT
      ClassHeader
      "=>"
      "."
      Modifiers
      ClassWord
      IDENTIFIER
      Extends
      Interfaces)
     (COMMENT TypeDeclaration "=>" "." ClassHeader LCURLY RCURLY)
     (COMMENT
      TypeDeclaration
      "=>"
      "."
      ClassHeader
      LCURLY
      FieldDeclarations
      RCURLY)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration SemiColons)
     (COMMENT TypeDeclarationOptSemi "=>" "." TypeDeclaration)
     (COMMENT
      TypeDeclarations
      "=>"
      TypeDeclarations
      "."
      TypeDeclarationOptSemi)
     (COMMENT
      ProgramFile
      "=>"
      PackageStatement
      ImportStatements
      TypeDeclarations
      ".")
     (SHIFT (ABSTRACT) s13)
     (SHIFT (CLASS) s14)
     (SHIFT (FINAL) s15)
     (SHIFT (INTERFACE) s17)
     (SHIFT (NATIVE) s18)
     (SHIFT (PRIVATE) s20)
     (SHIFT (PROTECTED) s21)
     (SHIFT (PUBLIC) s22)
     (SHIFT (STATIC) s23)
     (SHIFT (SYNCHRONIZED) s24)
     (SHIFT (TRANSIENT) s25)
     (SHIFT (VOLATILE) s26)
     (REDUCE (*EOF*) r20)
     (GOTO TypeDeclarationOptSemi s443)
     (GOTO TypeDeclaration s8)
     (GOTO ClassHeader s9)
     (GOTO Modifiers s10)
     (GOTO Modifier s11)
     (GOTO ClassWord s12))
    (STATE
     s447
     (COMMENT *start "=>" CompilationUnit *EOF* ".")
     (REDUCE () r1))))