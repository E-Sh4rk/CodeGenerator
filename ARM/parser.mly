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
  | id = ID ; args = separated_list(COMMA, arg) { ($startpos, id, args) }
  ;

arg:
  | id = ID { Register id }
  | HASH ; i = NUMBER { Immediate i }
  | LEFT_BRACK ; id = ID ; o = offset ; RIGHT_BRACK { Offset (id, o, false) }
  | LEFT_BRACK ; id = ID ; o = offset ; RIGHT_BRACK ; EXCLAM_MARK { Offset (id, o, true) }
  ;

offset:
  | (* Empty *) { ONone }
  | COMMA ; HASH ; i = NUMBER | COMMA ; HASH ; PLUS ; i = NUMBER
    { OImmediate (Arm.sign_plus, i) }
  | COMMA ; HASH ; MINUS ; i = NUMBER { OImmediate (Arm.sign_minus, i) }
  | COMMA ; id = ID | COMMA ; PLUS ; id = ID { ORegister (Arm.sign_plus, id) }
  | COMMA ; MINUS ; id = ID { ORegister (Arm.sign_minus, id) }
  ;