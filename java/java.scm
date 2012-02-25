(define java-pda
  (compile+convert-to-pda
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
            IDENTIFIER LITERAL BOOLLIT

            SEMICOLON COMMA LPAREN RPAREN LBRACK RBRACK
            LCURLY RCURLY
            EQUAL_SIGN DOT PLUS STAR FORWARD_SLASH MINUS PERCENT
            LESS_THAN GREATER_THAN COLON PIPE QMARK AMPERSAND CARROT
            BANG TWIDDLE)

    (start CompilationUnit)

    (non-term TypeSpecifier
              (=> (TypeName) 0)
              (=> (TypeName Dims) 0)
              )

    (non-term TypeName
              (=> (PrimitiveType) 0)
              (=> (QualifiedName) 0)
              )

    (non-term ClassNameList
              (=> (QualifiedName) 0)
              (=> (ClassNameList COMMA QualifiedName) 0)
              )

    (non-term PrimitiveType
              (=> (BOOLEAN) 0)
              (=> (CHAR) 0)
              (=> (BYTE) 0)
              (=> (SHORT) 0)
              (=> (INT) 0)
              (=> (LONG) 0)
              (=> (FLOAT) 0)
              (=> (DOUBLE) 0)
              (=> (VOID) 0)
              )

    (non-term SemiColons
     (=> (SEMICOLON) 0)
     (=> (SemiColons SEMICOLON) 0)
     )

    (non-term CompilationUnit
              (=> (ProgramFile) 0)
              )

    (non-term ProgramFile
              (=> (PackageStatement ImportStatements TypeDeclarations) 0)
              (=> (PackageStatement ImportStatements) 0)
              (=> (PackageStatement                  TypeDeclarations) 0)
              (=> (                 ImportStatements TypeDeclarations) 0)
              (=> (PackageStatement) 0)
              (=> (                 ImportStatements) 0)
              (=> (                                  TypeDeclarations) 0)
              )

    (non-term PackageStatement
              (=> (PACKAGE QualifiedName SemiColons) 0)
              )

    (non-term TypeDeclarations
              (=> (TypeDeclarationOptSemi) 0)
              (=> (TypeDeclarations TypeDeclarationOptSemi) 0)
              )

    (non-term TypeDeclarationOptSemi
              (=> (TypeDeclaration) 0)
              (=> (TypeDeclaration SemiColons) 0)
              )

    (non-term ImportStatements
              (=> (ImportStatement) 0)
              (=> (ImportStatements ImportStatement) 0)
              )

    (non-term ImportStatement
              (=> (IMPORT QualifiedName SemiColons) 0)
              (=> (IMPORT QualifiedName DOT STAR SemiColons) 0)
              )

    (non-term QualifiedName
              (=> (IDENTIFIER) 0)
              (=> (QualifiedName DOT IDENTIFIER) 0)
              )

    (non-term TypeDeclaration
              (=> (ClassHeader LCURLY FieldDeclarations RCURLY) 0)
              (=> (ClassHeader LCURLY RCURLY) 0)
              )

    (non-term ClassHeader
              (=> (Modifiers ClassWord IDENTIFIER Extends Interfaces) 0)
              (=> (Modifiers ClassWord IDENTIFIER Extends) 0)
              (=> (Modifiers ClassWord IDENTIFIER       Interfaces) 0)
              (=> (          ClassWord IDENTIFIER Extends Interfaces) 0)
              (=> (Modifiers ClassWord IDENTIFIER) 0)
              (=> (          ClassWord IDENTIFIER Extends) 0)
              (=> (          ClassWord IDENTIFIER       Interfaces) 0)
              (=> (          ClassWord IDENTIFIER) 0)
              )

    (non-term Modifiers
              (=> (Modifier) 0)
              (=> (Modifiers Modifier) 0)
              )

    (non-term Modifier
              (=> (ABSTRACT) 0)
              (=> (FINAL) 0)
              (=> (PUBLIC) 0)
              (=> (PROTECTED) 0)
              (=> (PRIVATE) 0)
              (=> (STATIC) 0)
              (=> (TRANSIENT) 0)
              (=> (VOLATILE) 0)
              (=> (NATIVE) 0)
              (=> (SYNCHRONIZED) 0)
              )

    (non-term ClassWord
              (=> (CLASS) 0)
              (=> (INTERFACE) 0)
              )

    (non-term Interfaces
              (=> (IMPLEMENTS ClassNameList) 0)
              )

    (non-term FieldDeclarations
              (=> (FieldDeclarationOptSemi) 0)
              (=> (FieldDeclarations FieldDeclarationOptSemi) 0)
              )

    (non-term FieldDeclarationOptSemi
              (=> (FieldDeclaration) 0)
              (=> (FieldDeclaration SemiColons) 0)
              )

    (non-term FieldDeclaration
              (=> (FieldVariableDeclaration SEMICOLON) 0)
              (=> (MethodDeclaration) 0)
              (=> (ConstructorDeclaration) 0)
              (=> (StaticInitializer) 0)
              (=> (NonStaticInitializer) 0)
              (=> (TypeDeclaration) 0)
              )

    (non-term FieldVariableDeclaration
              (=> (Modifiers TypeSpecifier VariableDeclarators) 0)
              (=> (          TypeSpecifier VariableDeclarators) 0)
              )

    (non-term VariableDeclarators
              (=> (VariableDeclarator) 0)
              (=> (VariableDeclarators COMMA VariableDeclarator) 0)
              )

    (non-term VariableDeclarator
              (=> (DeclaratorName) 0)
              (=> (DeclaratorName EQUAL_SIGN VariableInitializer) 0)
              )

    (non-term VariableInitializer
              (=> (Expression) 0)
              (=> (LCURLY RCURLY) 0)
              (=> (LCURLY ArrayInitializers RCURLY) 0)
              )

    (non-term ArrayInitializers
              (=> (VariableInitializer) 0)
              (=> (ArrayInitializers COMMA VariableInitializer) 0)
              (=> (ArrayInitializers COMMA) 0)
              )

    (non-term MethodDeclaration
              (=> (Modifiers TypeSpecifier MethodDeclarator Throws MethodBody) 0)
              (=> (Modifiers TypeSpecifier MethodDeclarator        MethodBody) 0)
              (=> (          TypeSpecifier MethodDeclarator Throws MethodBody) 0)
              (=> (          TypeSpecifier MethodDeclarator        MethodBody) 0)
              )

    (non-term MethodDeclarator
              (=> (DeclaratorName LPAREN ParameterList RPAREN) 0)
              (=> (DeclaratorName LPAREN RPAREN) 0)
              (=> (MethodDeclarator OP_DIM) 0)
              )

    (non-term ParameterList
              (=> (Parameter) 0)
              (=> (ParameterList COMMA Parameter) 0)
              )

    (non-term Parameter
              (=> (TypeSpecifier DeclaratorName) 0)
              (=> (FINAL TypeSpecifier DeclaratorName) 0)
              )

    (non-term DeclaratorName
              (=> (IDENTIFIER) 0)
              (=> (DeclaratorName OP_DIM) 0)
              )

    (non-term Throws
              (=> (THROWS ClassNameList) 0)
              )

    (non-term MethodBody
              (=> (Block) 0)
              (=> (SEMICOLON) 0)
              )

    (non-term ConstructorDeclaration
              (=> (Modifiers ConstructorDeclarator Throws Block) 0)
              (=> (Modifiers ConstructorDeclarator        Block) 0)
              (=> (          ConstructorDeclarator Throws Block) 0)
              (=> (          ConstructorDeclarator        Block) 0)
              )

    (non-term ConstructorDeclarator
              (=> (IDENTIFIER LPAREN ParameterList RPAREN) 0)
              (=> (IDENTIFIER LPAREN RPAREN) 0)
              )

    (non-term StaticInitializer
              (=> (STATIC Block) 0)
              )

    (non-term NonStaticInitializer
              (=> (Block) 0)
              )

    (non-term Extends
              (=> (EXTENDS TypeName) 0)
              (=> (Extends COMMA TypeName) 0)
              )

    (non-term Block
              (=> (LCURLY LocalVariableDeclarationsAndStatements RCURLY) 0)
              (=> (LCURLY RCURLY) 0)
              )

    (non-term LocalVariableDeclarationsAndStatements
              (=> (LocalVariableDeclarationOrStatement) 0)
              (=> (LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement) 0)
              )

    (non-term LocalVariableDeclarationOrStatement
              (=> (LocalVariableDeclarationStatement) 0)
              (=> (Statement) 0)
              )

    (non-term LocalVariableDeclarationStatement
              (=> (TypeSpecifier VariableDeclarators SEMICOLON) 0)
              (=> (FINAL TypeSpecifier VariableDeclarators SEMICOLON) 0)
              )

    (non-term Statement
              (=> (EmptyStatement) 0)
              (=> (LabelStatement) 0)
              (=> (ExpressionStatement SEMICOLON) 0)
              (=> (SelectionStatement) 0)
              (=> (IterationStatement) 0)
              (=> (JumpStatement) 0)
              (=> (GuardingStatement) 0)
              (=> (Block) 0)
              )

    (non-term EmptyStatement
              (=> (SEMICOLON) 0)
              )

    (non-term LabelStatement
              (=> (IDENTIFIER COLON) 0)
              (=> (CASE ConstantExpression COLON) 0)
              (=> (DEFAULT COLON) 0)
              )

    (non-term ExpressionStatement
              (=> (Expression) 0)
              )

    (non-term SelectionStatement
              (=> (IF LPAREN Expression RPAREN Statement) 0)
              (=> (IF LPAREN Expression RPAREN Statement ELSE Statement) 0)
              (=> (SWITCH LPAREN Expression RPAREN Block) 0)
              )

    (non-term IterationStatement
              (=> (WHILE LPAREN Expression RPAREN Statement) 0)
              (=> (DO Statement WHILE LPAREN Expression RPAREN SEMICOLON) 0)
              (=> (FOR LPAREN ForInit ForExpr ForIncr RPAREN Statement) 0)
              (=> (FOR LPAREN ForInit ForExpr         RPAREN Statement) 0)
              )

    (non-term ForInit
              (=> (ExpressionStatements SEMICOLON) 0)
              (=> (LocalVariableDeclarationStatement) 0)
              (=> (SEMICOLON) 0)
              )

    (non-term ForExpr
              (=> (Expression SEMICOLON) 0)
              (=> (SEMICOLON) 0)
              )

    (non-term ForIncr
              (=> (ExpressionStatements) 0)
              )

    (non-term ExpressionStatements
              (=> (ExpressionStatement) 0)
              (=> (ExpressionStatements COMMA ExpressionStatement) 0)
              )

    (non-term JumpStatement
              (=> (BREAK IDENTIFIER SEMICOLON) 0)
              (=> (BREAK            SEMICOLON) 0)
              (=> (CONTINUE IDENTIFIER SEMICOLON) 0)
              (=> (CONTINUE            SEMICOLON) 0)
              (=> (RETURN Expression SEMICOLON) 0)
              (=> (RETURN            SEMICOLON) 0)
              (=> (THROW Expression SEMICOLON) 0)
              )

    (non-term GuardingStatement
              (=> (SYNCHRONIZED LPAREN Expression RPAREN Statement) 0)
              (=> (TRY Block Finally) 0)
              (=> (TRY Block Catches) 0)
              (=> (TRY Block Catches Finally) 0)
              )

    (non-term Catches
              (=> (Catch) 0)
              (=> (Catches Catch) 0)
              )

    (non-term Catch
              (=> (CatchHeader Block) 0)
              )

    (non-term CatchHeader
              (=> (CATCH LPAREN TypeSpecifier IDENTIFIER RPAREN) 0)
              (=> (CATCH LPAREN TypeSpecifier RPAREN) 0)
              )

    (non-term Finally
              (=> (FINALLY Block) 0)
              )

    (non-term PrimaryExpression
              (=> (QualifiedName) 0)
              (=> (NotJustName) 0)
              )

    (non-term NotJustName
              (=> (SpecialName) 0)
              (=> (NewAllocationExpression) 0)
              (=> (ComplexPrimary) 0)
              )

    (non-term ComplexPrimary
              (=> (LPAREN Expression RPAREN) 0)
              (=> (ComplexPrimaryNoParenthesis) 0)
              )

    (non-term ComplexPrimaryNoParenthesis
              (=> (LITERAL) 0)
              (=> (BOOLLIT) 0)
              (=> (ArrayAccess) 0)
              (=> (FieldAccess) 0)
              (=> (MethodCall) 0)
              )

    (non-term ArrayAccess
              (=> (QualifiedName LBRACK Expression RBRACK) 0)
              (=> (ComplexPrimary LBRACK Expression RBRACK) 0)
              )

    (non-term FieldAccess
              (=> (NotJustName DOT IDENTIFIER) 0)
              (=> (RealPostfixExpression DOT IDENTIFIER) 0)
              (=> (QualifiedName DOT THIS) 0)
              (=> (QualifiedName DOT CLASS) 0)
              (=> (PrimitiveType DOT CLASS) 0)
              )

    (non-term MethodCall
              (=> (MethodAccess LPAREN ArgumentList RPAREN) 0)
              (=> (MethodAccess LPAREN RPAREN) 0)
              )

    (non-term MethodAccess
              (=> (ComplexPrimaryNoParenthesis) 0)
              (=> (SpecialName) 0)
              (=> (QualifiedName) 0)
              )

    (non-term SpecialName
              (=> (THIS) 0)
              (=> (SUPER) 0)
              (=> (JNULL) 0)
              )

    (non-term ArgumentList
              (=> (Expression) 0)
              (=> (ArgumentList COMMA Expression) 0)
              )

    (non-term NewAllocationExpression
              (=> (PlainNewAllocationExpression) 0)
              (=> (QualifiedName DOT PlainNewAllocationExpression) 0)
              )

    (non-term PlainNewAllocationExpression
              (=> (ArrayAllocationExpression) 0)
              (=> (ClassAllocationExpression) 0)
              (=> (ArrayAllocationExpression LCURLY RCURLY) 0)
              (=> (ClassAllocationExpression LCURLY RCURLY) 0)
              (=> (ArrayAllocationExpression LCURLY ArrayInitializers RCURLY) 0)
              (=> (ClassAllocationExpression LCURLY FieldDeclarations RCURLY) 0)
              )

    (non-term ClassAllocationExpression
              (=> (NEW TypeName LPAREN ArgumentList RPAREN) 0)
              (=> (NEW TypeName LPAREN              RPAREN) 0)
              )

    (non-term ArrayAllocationExpression
              (=> (NEW TypeName DimExprs Dims) 0)
              (=> (NEW TypeName DimExprs) 0)
              (=> (NEW TypeName Dims) 0)
              )

    (non-term DimExprs
              (=> (DimExpr) 0)
              (=> (DimExprs DimExpr) 0)
              )

    (non-term DimExpr
              (=> (LBRACK Expression RBRACK) 0)
              )

    (non-term Dims
              (=> (OP_DIM) 0)
              (=> (Dims OP_DIM) 0)
              )

    (non-term PostfixExpression
              (=> (PrimaryExpression) 0)
              (=> (RealPostfixExpression) 0)
              )

    (non-term RealPostfixExpression
              (=> (PostfixExpression OP_INC) 0)
              (=> (PostfixExpression OP_DEC) 0)
              )

    (non-term UnaryExpression
              (=> (OP_INC UnaryExpression) 0)
              (=> (OP_DEC UnaryExpression) 0)
              (=> (ArithmeticUnaryOperator CastExpression) 0)
              (=> (LogicalUnaryExpression) 0)
              )

    (non-term LogicalUnaryExpression
              (=> (PostfixExpression) 0)
              (=> (LogicalUnaryOperator UnaryExpression) 0)
              )

    (non-term LogicalUnaryOperator
              (=> (TWIDDLE) 0)
              (=> (BANG) 0)
              )

    (non-term ArithmeticUnaryOperator
              (=> (PLUS) 0)
              (=> (MINUS) 0)
              )

    (non-term CastExpression
              (=> (UnaryExpression) 0)
              (=> (LPAREN PrimitiveTypeExpression RPAREN CastExpression) 0)
              (=> (LPAREN ClassTypeExpression RPAREN CastExpression) 0)
              (=> (LPAREN Expression RPAREN LogicalUnaryExpression) 0)
              )

    (non-term PrimitiveTypeExpression
              (=> (PrimitiveType) 0)
              (=> (PrimitiveType Dims) 0)
              )

    (non-term ClassTypeExpression
              (=> (QualifiedName Dims) 0)
              )

    (non-term MultiplicativeExpression
              (=> (CastExpression) 0)
              (=> (MultiplicativeExpression STAR CastExpression) 0)
              (=> (MultiplicativeExpression FORWARD_SLASH CastExpression) 0)
              (=> (MultiplicativeExpression PERCENT CastExpression) 0)
              )

    (non-term AdditiveExpression
              (=> (MultiplicativeExpression) 0)
              (=> (AdditiveExpression PLUS MultiplicativeExpression) 0)
              (=> (AdditiveExpression MINUS MultiplicativeExpression) 0)
              )

    (non-term ShiftExpression
              (=> (AdditiveExpression) 0)
              (=> (ShiftExpression OP_SHL AdditiveExpression) 0)
              (=> (ShiftExpression OP_SHR AdditiveExpression) 0)
              (=> (ShiftExpression OP_SHRR AdditiveExpression) 0)
              )

    (non-term RelationalExpression
              (=> (ShiftExpression) 0)
              (=> (RelationalExpression LESS_THAN ShiftExpression) 0)
              (=> (RelationalExpression GREATER_THAN ShiftExpression) 0)
              (=> (RelationalExpression OP_LE ShiftExpression) 0)
              (=> (RelationalExpression OP_GE ShiftExpression) 0)
              (=> (RelationalExpression INSTANCEOF TypeSpecifier) 0)
              )

    (non-term EqualityExpression
              (=> (RelationalExpression) 0)
              (=> (EqualityExpression OP_EQ RelationalExpression) 0)
              (=> (EqualityExpression OP_NE RelationalExpression) 0)
              )

    (non-term AndExpression
              (=> (EqualityExpression) 0)
              (=> (AndExpression AMPERSAND EqualityExpression) 0)
              )

    (non-term ExclusiveOrExpression
              (=> (AndExpression) 0)
              (=> (ExclusiveOrExpression CARROT AndExpression) 0)
              )

    (non-term InclusiveOrExpression
              (=> (ExclusiveOrExpression) 0)
              (=> (InclusiveOrExpression PIPE ExclusiveOrExpression) 0)
              )

    (non-term ConditionalAndExpression
              (=> (InclusiveOrExpression) 0)
              (=> (ConditionalAndExpression OP_LAND InclusiveOrExpression) 0)
              )

    (non-term ConditionalOrExpression
              (=> (ConditionalAndExpression) 0)
              (=> (ConditionalOrExpression OP_LOR ConditionalAndExpression) 0)
              )

    (non-term ConditionalExpression
              (=> (ConditionalOrExpression) 0)
              (=> (ConditionalOrExpression QMARK Expression COLON ConditionalExpression) 0)
              )

    (non-term AssignmentExpression
              (=> (ConditionalExpression) 0)
              (=> (UnaryExpression AssignmentOperator AssignmentExpression) 0)
              )

    (non-term AssignmentOperator
              (=> (EQUAL_SIGN) 0)
              (=> (ASS_MUL) 0)
              (=> (ASS_DIV) 0)
              (=> (ASS_MOD) 0)
              (=> (ASS_ADD) 0)
              (=> (ASS_SUB) 0)
              (=> (ASS_SHL) 0)
              (=> (ASS_SHR) 0)
              (=> (ASS_SHRR) 0)
              (=> (ASS_AND) 0)
              (=> (ASS_XOR) 0)
              (=> (ASS_OR) 0)
              )

    (non-term Expression
              (=> (AssignmentExpression) 0)
              )

    (non-term ConstantExpression
              (=> (ConditionalExpression) 0)
              )
    )))
