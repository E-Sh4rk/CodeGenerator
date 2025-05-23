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
%token COLON
%token TIMES
%token DIV
%token MOD
%token PLUS
%token MINUS
%token AND
%token OR
%token XOR
%token NOT
%token EQ
%token NEQ
%token BOR
%token BAND
%token LSL LSR ASR ROR RRX
%token LEQ GEQ LT GT
%token EOL
%token EOF

%nonassoc ARG
%nonassoc COMMA

%right INTERROG_MARK COLON
%left BOR
%left BAND
%left OR
%left XOR
%left AND
%left EQ NEQ LEQ GEQ LT GT
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc EXCLAM_MARK NOT UPLUS UMINUS

%start <Parser_ast.ast> ast
%start <Preprocess.headers> headers
%%

%inline binop:
| MOD { OMod } | DIV { ODiv } | TIMES { OMul } | MINUS { OSub } | PLUS { OAdd }
| BAND { OBAnd } | BOR { OBOr } | OR { OOr } | XOR { OXor } | AND { OAnd }
| EQ { OEq } | NEQ { ONeq }
| LT { OLt } | GT { OGt } | LEQ { OLeq } | GEQ { OGeq }
| LSHIFT { OLShift } | RSHIFT { ORShift }

meta_expr:
  | i = NUMBER { MConst i }
  | v = ID { MVar v }
  | LPAREN e = meta_expr RPAREN { e }
  | e1 = meta_expr bop = binop e2 = meta_expr { MBinary (bop, e1, e2) }
  | PLUS e = meta_expr %prec UPLUS { MUnary (OId, e) }
  | MINUS e = meta_expr %prec UMINUS { MUnary (ONeg, e) }
  | NOT e = meta_expr { MUnary (ONot, e) }
  | EXCLAM_MARK e = meta_expr { MUnary (OBNot, e) }
  | e0 = meta_expr INTERROG_MARK e1 = meta_expr COLON e2 = meta_expr
  { MCond (e0, e1, e2) }

definition:
  | HEADER ; id = ID ; EQUAL ; str = STRING { Param (id, HString str) }
  | HEADER ; id = ID ; EQUAL ; nb = NUMBER { Param (id, HInt nb) }
  | HEADER ; id = ID ; EQUAL ; b = BOOL { Param (id, HBool b) }
  | HEADER ; id = ID ; EQUAL ; NULL { Param (id, HNone) }
  | id = ID ; EQUAL ; e = meta_expr { VarDef (id, false, e) }
  | id = ID ; INTERROG_MARK ; EQUAL ; e = meta_expr { VarDef (id, true, e) }
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
  | id = ID ; args = separated_list(COMMA, arg) { ASM ($startpos, id, args, Optimizer.NoTweaking) }
  | id = ID ; args = separated_list(COMMA, arg) ; INTERROG_MARK { ASM ($startpos, id, args, Optimizer.TweakMinLength) }
  | id = ID ; args = separated_list(COMMA, arg) ; INTERROG_MARK ; i = NUMBER
  { ASM ($startpos, id, args, Optimizer.TweakFixedLength (Utils.uint32_to_int i)) }
  | nb = number { BIN ($startpos, nb) }
  ;

%inline maybe_hash:
  | {}
  | HASH {}
  ;

%inline imm:
  | maybe_hash ; i = number { i }
  ;

%inline imm_or_reg:
  | i=imm { Imm i } | id=ID { Reg id }
  ;

%inline oshift:
  | LSL ; i = imm { LSL i } | LSR ; i = imm { LSR i } | ASR ; i = imm { ASR i }
  | ROR ; i = imm { ROR i } | RRX { RRX }
  ;

%inline shift:
  | LSL ; i = imm_or_reg { LSL i } | LSR ; i = imm_or_reg { LSR i } | ASR ; i = imm_or_reg { ASR i }
  | ROR ; i = imm_or_reg { ROR i } | RRX { RRX }
  ;

%inline offset_arg:
  | LEFT_BRACK ; id = ID ; COMMA ; o = offset ; RIGHT_BRACK
  { (id, o, Arm.Offset) }
  | LEFT_BRACK ; id = ID ; RIGHT_BRACK %prec ARG
  { (id, OImmediate (Arm.sign_plus, ConstInt32 Int32.zero), Arm.Offset) }
  | LEFT_BRACK ; id = ID ; COMMA ; o = offset ; RIGHT_BRACK ; EXCLAM_MARK
  { (id, o, Arm.PreIndexed) }
  | LEFT_BRACK ; id = ID ; RIGHT_BRACK ; EXCLAM_MARK
  { (id, OImmediate (Arm.sign_plus, ConstInt32 Int32.zero), Arm.PreIndexed) }
  | LEFT_BRACK ; id = ID ; RIGHT_BRACK ; COMMA ; o = offset
  { (id, o, Arm.PostIndexed) }
  ;

arg:
  | id = ID { Register id }
  | i = imm { Immediate i }
  | s = shift { Shift s }
  | o = offset_arg { let (str,o,at) = o in Offset (str,o,at) }
  ;

offset:
  | maybe_hash ; PLUS? ; i = number { OImmediate (Arm.sign_plus, i) }
  | maybe_hash ; MINUS ; i = number { OImmediate (Arm.sign_minus, i) }
  | PLUS? ; id = ID %prec ARG { ORegister (Arm.sign_plus, id) }
  | MINUS ; id = ID %prec ARG { ORegister (Arm.sign_minus, id) }
  | PLUS? ; id = ID ; COMMA ; s=oshift { OShift (Arm.sign_plus, id, s) }
  | MINUS ; id = ID ; COMMA ; s=oshift { OShift (Arm.sign_minus, id, s) }
  ;

number:
  | i = NUMBER { ConstInt32 i }
  | LEFT_BRACE ; e=meta_expr ; RIGHT_BRACE { MetaExpr e }
  ;