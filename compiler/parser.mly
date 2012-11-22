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
		var_type ID LPAREN vdecl_list RPAREN LBRACE cont_list RBRACE
		
		
var_type:
		(INT | STR | BOOL)	{ [$1] }
		
vdecl_list:
		/* nothing */				{ [] }
	| vdecl_list vdecl		{ $2 :: $1 }

vdecl:
		var_type ID SEMI		{ ($1, $2) }