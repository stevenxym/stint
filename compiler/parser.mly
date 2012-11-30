%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
			 LBRACK RBRACK LPANGLE LANGLE RANGLE												(* marks *)
%token ASSIGN PLUS MINUS TIMES DIVIDE EQ NEQ LESS LEQ GRT GEQ			(* general operators *)
%token AT SPLIT SEARCH RM NOT AND OR	COUT CIN										(* type-specified operators *)
%token INT STR BOOL IF WHILE RETURN OPEN CLOSE BREAK EOF
			 VOID TRUE FALSE STD																				(* key word *)
%token END		(* don't know whether need it *)
%token <int> LIT_INT
%token <string> LIT_STR
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc AT
%nonassoc NOAT
%right ASSIGN COUT CIN					(* =, <<, >> *)
%left EQ NEQ										(* ==, != *)
%left LESS LEQ GRT GEQ					(* <, <=, >, >= *)
%left AND OR
%right RM										(* &&, || *)
%right NOT											(* ! *)
%left PLUS MINUS								(* +, - *)
%left TIMES DIVIDE							(* *, / *)										(* @, ~ *)
%left SPLIT SEARCH							(* |, # *)

%start program
%type <Ast.program> program

%%

program:
		/* nothing */				{ [], [] }
	| program vdecl				{ ($2 :: fst $1), snd $1 }
	| program fdecl				{ fst $1, ($2 :: snd $1) }

fdecl:
		var_type ID LPAREN formals_opt RPAREN LBRACE cont_list RBRACE
		{
			{
				returnType = $1;
				fname = $2;
				formals = $4
				body = List.rev $7
			}
		}
		
var_type:
		(INT | STR | BOOL)	{ [$1] }
		
formals_opt:
		/* nothing */ { [] }
	| formal_list { List.rev $1 }

formal_list:
	| var_type ID { [($1, $2)] }
	| formal_list COMMA var_type ID { ($3, $4) :: $1}

vdecl:
		var_type ID SEMI { ($1, $2) }

vdecl_list:
		/* EMPTY */	{ [] }
		| vdecl_list vdecl { $2 :: $1 } 

cont_list:
		/* EMPTY */ { [] }
		| cont_list stmt { $2 :: $1 }
		| cont_list vdecl { $2 :: $1}

stmt:
		expr SEMI { Expr($1) }
		| RETURN expr SEMI { Return($2) }
		| LBRACE stmt_list RBRACE { Block(List.rev $2) }
		| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
		| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
		| WHILE LPAREN expr RPAREN stmt { While($3, $5) }
		| BREAK SEMI { Break }
		| OPEN expr SEMI { Fop(Open, $2) }
		| CLOSE expr SEMI { Fop(Close, $2) }

expr:
		LIT_INT { Integer($1) }
		| ID { Id($1) }
		| expr PLUS expr %prec NOAT { Oper($1, Add, $3) }
		| expr MINUS expr %prec NOAT { Oper($1, Sub, $3) }
		| expr PLUS expr AT expr{ OperAt($1, Add, at, $5) }
		| expr MINUS expr AT expr{ OperAt($1, Sub, at, $5) }
		| expr TIMES expr { Oper($1, Mult, $3) }
		| expr DIVIDE expr { Oper($1, Div, $3) }
		| expr EQ expr { Oper($1, Equal, $3) }
		| expr NEQ expr { Oper($1, Neq, $3) }
		| expr LESS expr { Oper($1, Less, $3) }
		| expr LEQ expr { Oper($1, LessEq, $3) }
		| expr GRT expr { Oper($1, Grt, $3) }
		| expr GEQ expr { Oper($1, GrtEq, $3) }
		| ID ASSIGN expr { Assign($1, $3) }
		| expr LBRACK LIT_INT RBRACK { Extract($1, SubChar, $3) }
		| expr { Extract($1, SubChar, $3) }
		| expr LBRACK LIT_INT RBRACK { Extract($1, SubChar, $3) }
		| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
		| LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }








