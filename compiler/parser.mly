%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACK RBRACK LPANGLE LANGLE RANGLE	/* marks */
%token ASSIGN PLUS MINUS TIMES DIVIDE EQ NEQ LESS LEQ GRT GEQ			  	/* general operators */
%token AT SPLIT SEARCH RM NOT AND OR COUT CIN					/* type-specified operators */
%token INT STR BOOL IF ELSE WHILE RETURN OPEN CLOSE BREAK EOF VOID TRUE FALSE STD	/* key word */
%token END					/* don't know whether need it */
%token <int> LIT_INT
%token <string> LIT_STR
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc NOAT
%nonassoc AT

%nonassoc COUT CIN
%right ASSIGN			/* =, <<, >> */
%left EQ NEQ			/* ==, != */
%left LESS LEQ GRT GEQ		/* <, <=, >, >= */
%left AND OR			/* &&, || */
%right RM			/* ~ */
%right NOT			/* ! */
%left PLUS MINUS		/* +, - */
%left TIMES DIVIDE		/* *, / */
%left SPLIT SEARCH		/* |, # */
%nonassoc LPANGLE LANGLE RANGLE /* <| |> .<| */


%start program
%type <Ast.program> program

%%

program:
	/* nothing */				{ [], [] }
	| program vdecl SEMI			{ ($2 :: fst $1), snd $1 }
	| program fdecl				{ fst $1, ($2 :: snd $1) }

fdecl:
		var_type ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
		{
			{
				returnType = $1;
				fname = $2;
				formals = $4;
				body = List.rev $7
			}
		}
		
var_type:
		INT { "int" }
		| STR { "string" }
		| BOOL { "boolean"}
		
formals_opt:
	/* nothing */ { [] }
	| formal_list { List.rev $1 }

vdecl:
	var_type ID { ($1, $2, Noexpr) }
	| var_type ID ASSIGN expr { ($1, $2, $4) }

formal_list:
	| vdecl { [$1] }
	| formal_list COMMA vdecl { $3 :: $1}

/*
cont_list:
		 { [] }
		| cont_list stmt { $2 :: $1 }
		| cont_list vdecl { $2 :: $1}
*/

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
		| vdecl SEMI { Decl($1) }

stmt_list:
		/*Nothing*/ { [] }
		|stmt_list stmt {$2 :: $1}

expr:
		LIT_INT { Integer($1) }
		|LIT_STR { String($1) }
		| ID { Id($1) }
		/*| STD { Std("std") }*/
		/* ___Operator___ */
		| expr PLUS expr %prec NOAT { Oper($1, Add, $3) }
		| expr MINUS expr %prec NOAT { Oper($1, Sub, $3) }
		| expr PLUS expr AT expr{ OperAt($1, Add, $3, $5) }
		| expr MINUS expr AT expr{ OperAt($1, Sub, $3, $5) }
		| expr TIMES expr { Oper($1, Mult, $3) }
		| expr DIVIDE expr { Oper($1, Div, $3) }
		| expr EQ expr { Oper($1, Equal, $3) }
		| expr NEQ expr { Oper($1, Neq, $3) }
		| expr LESS expr { Oper($1, Less, $3) }
		| expr LEQ expr { Oper($1, LessEq, $3) }
		| expr GRT expr { Oper($1, Grt, $3) }
		| expr GEQ expr { Oper($1, GrtEq, $3) }
		/* ___Extract___ */
		| ID LBRACK expr RBRACK { Extract($1, SubChar, $3) } 
		| ID LPANGLE expr RANGLE { Extract($1, SubInt, $3) } 
		| ID LANGLE expr RANGLE { Extract($1, SubStr, $3) } 
		| ID LBRACK expr COMMA expr RBRACK { Sublen($1, $3, $5) }
		/* ___Assign___ */
		| ID ASSIGN expr { Assign($1, $3) }
		| ID LANGLE expr RANGLE ASSIGN expr { AssignSet($1, SubStr, $3, $6) }
		| ID LPANGLE expr RANGLE ASSIGN expr { AssignSet($1, SubInt, $3, $6) }
		| ID LBRACK expr COMMA expr RBRACK ASSIGN expr { AssignSet($1, SubStr, $3, $8) }

		| ID SEARCH expr { Chset($1, Fnd, $3) }
		| ID SPLIT expr { Chset($1, Spl, $3) }

		/* ___Remove___ */
		| RM ID LPANGLE expr RANGLE { RemoveSet($2, SubInt, $4) } 
		| RM ID LANGLE expr RANGLE { RemoveSet($2, SubStr, $4) }
		| RM ID LBRACK expr COMMA expr RBRACK { RemoveStr($2, $4, $6) }
		/* ____Stream___ */
		| LIT_STR CIN expr { Stream(In, $1, $3) }
		| STD CIN expr { Stream(In, "std", $3) }
		| LIT_STR COUT expr { Stream(Out, $1, $3) }
		| STD COUT expr { Stream(Out, "std", $3) }

		| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
		| LPAREN expr RPAREN { $2 }

actuals_opt: 
    /* nothing */  { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
