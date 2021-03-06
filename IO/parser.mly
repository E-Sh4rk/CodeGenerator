%{ open Parser_ast %}
%{ open Preprocess %}

%token HEADER
%token NULL
%token EQUAL
%token <string> STRING
%token <int32> NUMBER
%token <bool> BOOL
%token <string> ID
%token HASH
%token LEFT_BRACK
%token RIGHT_BRACK
%token LEFT_BRACE
%token RIGHT_BRACE
%token LPAREN
%token RPAREN
%token LSHIFT
%token RSHIFT
%token COMMA
%token EXCLAM_MARK
%token INTERROG_MARK
%token TIMES
%token DIV
%token MOD
%token PLUS
%token MINUS
%token AND
%token OR
%token XOR
%token NOT
%token EOL
%token EOF

%left OR
%left XOR
%left AND
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT
%nonassoc UPLUS UMINUS

%start <Parser_ast.ast> ast
%start <Preprocess.headers> headers
%%

meta_expr:
  | i = NUMBER { MConst i }
  | v = ID { MVar v }
  | LPAREN e = meta_expr RPAREN { e }
  | e1 = meta_expr PLUS e2 = meta_expr { MBinary (OAdd, e1, e2) }
  | e1 = meta_expr MINUS e2 = meta_expr { MBinary (OSub, e1, e2) }
  | e1 = meta_expr TIMES e2 = meta_expr { MBinary (OMul, e1, e2) }
  | e1 = meta_expr DIV e2 = meta_expr { MBinary (ODiv, e1, e2) }
  | e1 = meta_expr MOD e2 = meta_expr { MBinary (OMod, e1, e2) }
  | e1 = meta_expr AND e2 = meta_expr { MBinary (OAnd, e1, e2) }
  | e1 = meta_expr XOR e2 = meta_expr { MBinary (OXor, e1, e2) }
  | e1 = meta_expr OR e2 = meta_expr { MBinary (OOr, e1, e2) }
  | e1 = meta_expr LSHIFT e2 = meta_expr { MBinary (OLShift, e1, e2) }
  | e1 = meta_expr RSHIFT e2 = meta_expr { MBinary (ORShift, e1, e2) }
  | PLUS e = meta_expr %prec UPLUS { MUnary (OId, e) }
  | MINUS e = meta_expr %prec UMINUS { MUnary (ONeg, e) }
  | NOT e = meta_expr { MUnary (ONot, e) }

definition:
  | HEADER ; id = ID ; EQUAL ; str = STRING { Param (id, HString str) }
  | HEADER ; id = ID ; EQUAL ; nb = NUMBER { Param (id, HInt nb) }
  | HEADER ; id = ID ; EQUAL ; b = BOOL { Param (id, HBool b) }
  | HEADER ; id = ID ; EQUAL ; NULL { Param (id, HNone) }
  | id = ID ; EQUAL ; e = meta_expr { VarDef (id, e) }
  ;

headers:
  | list (EOL) ; HEADER ; EOL | list (EOL) ; HEADER ; EOF { [] }
  | list (EOL) ; d = definition ; EOL ; ds = headers { d::ds }
  ;

ast:
  | list (EOL) ; EOF { [] }
  | list (EOL) ; cmd = command ; EOL ; cmds = ast { cmd::cmds }
  | list (EOL) ; cmd = command ; EOF { [cmd] }
  ;

command:
  | id = ID ; args = separated_list(COMMA, arg) { ASM ($startpos, id, args, Optimizer.NoOptimization) }
  | id = ID ; args = separated_list(COMMA, arg) ; INTERROG_MARK { ASM ($startpos, id, args, Optimizer.VariableLength) }
  | id = ID ; args = separated_list(COMMA, arg) ; INTERROG_MARK ; i = NUMBER
  { ASM ($startpos, id, args, Optimizer.FixedLength (Name.int32_to_int i)) }
  | nb = number { BIN ($startpos, nb) }
  ;

arg:
  | id = ID { Register id }
  | HASH ; i = number | i = number { Immediate i }
  | LEFT_BRACK ; id = ID ; COMMA ; o = offset ; RIGHT_BRACK { Offset (id, o, Arm.Offset) }
  | LEFT_BRACK ; id = ID ; COMMA ; o = offset ; RIGHT_BRACK ; EXCLAM_MARK { Offset (id, o, Arm.PreIndexed) }
  | LEFT_BRACK ; id = ID ; RIGHT_BRACK ; COMMA ; o = offset { Offset (id, o, Arm.PostIndexed) }
  ;

offset:
  | HASH ; i = number | HASH ; PLUS ; i = number | i = number | PLUS ; i = number
  { OImmediate (Arm.sign_plus, i) }
  | HASH ; MINUS ; i = number | MINUS ; i = number { OImmediate (Arm.sign_minus, i) }
  | id = ID | PLUS ; id = ID { ORegister (Arm.sign_plus, id) }
  | MINUS ; id = ID { ORegister (Arm.sign_minus, id) }
  ;

number:
  | i = NUMBER { ConstInt32 i }
  | LEFT_BRACE ; e=meta_expr ; RIGHT_BRACE { MetaExpr e }
  ;