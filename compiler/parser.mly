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
%right ASSIGN COUT CIN					(* =, <<, >> *)
%left EQ NEQ										(* ==, != *)
%left LESS LEQ GRT GEQ					(* <, <=, >, >= *)
%left AND OR										(* &&, || *)
%right NOT											(* ! *)
%left PLUS MINUS								(* +, - *)
%left TIMES DIVIDE							(* *, / *)
%right AT RM										(* @, ~ *)
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
	| formal_list vdecl	{ List.rev $1 }

formal_list:
	| var_type ID { [$1] }
	| formal_list COMMA var_type { $3 :: $1}

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
		| FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
		| WHILE LPAREN expr RPAREN stmt { While($3, $5) }














