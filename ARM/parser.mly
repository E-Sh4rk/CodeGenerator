%{ open Parser_ast %}

%token <int32> NUMBER
%token <string> ID
%token HASH
%token LEFT_BRACK
%token RIGHT_BRACK
%token COMMA
%token EXCLAM_MARK
%token PLUS
%token MINUS
%token EOL
%token EOF

%start <Parser_ast.ast> ast
%%

ast:
  | list (EOL) ; EOF { [] }
  | list (EOL) ; cmd = command ; EOL ; cmds = ast { cmd::cmds }
  | list (EOL) ; cmd = command ; EOF { [cmd] }
  ;

command:
  | id = ID ; args = separated_list(COMMA, arg) { ASM ($startpos, id, args) }
  | nb = NUMBER { BIN ($startpos, nb) }
  ;

arg:
  | id = ID { Register id }
  | HASH ; i = NUMBER { Immediate i }
  | LEFT_BRACK ; id = ID ; COMMA ; o = offset ; RIGHT_BRACK { Offset (id, o, Arm.Offset) }
  | LEFT_BRACK ; id = ID ; COMMA ; o = offset ; RIGHT_BRACK ; EXCLAM_MARK { Offset (id, o, Arm.PreIndexed) }
  | LEFT_BRACK ; id = ID ; RIGHT_BRACK ; COMMA ; o = offset { Offset (id, o, Arm.PostIndexed) }
  ;

offset:
  | HASH ; i = NUMBER | HASH ; PLUS ; i = NUMBER { OImmediate (Arm.sign_plus, i) }
  | HASH ; MINUS ; i = NUMBER { OImmediate (Arm.sign_minus, i) }
  | id = ID | PLUS ; id = ID { ORegister (Arm.sign_plus, id) }
  | MINUS ; id = ID { ORegister (Arm.sign_minus, id) }
  ;