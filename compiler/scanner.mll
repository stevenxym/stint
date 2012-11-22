{ open Parser }

let lowLetter = ['a' - 'z']
let upLetter = ['A' - 'Z']
let digit = ['0' - '9']
let quote = '"'

rule token = parse
		[' ' '\t' '\r' '\n']	{ token lexbuf }
	| "/*"									{ comment lexbuf }
	| '('										{ LPAREN }					| ')'							{ RPAREN }
	| '{'										{ LBRACE }					| '}'							{ RBRACE }
	| ';'										{ SEMI }						| ','							{ COMMA }
	| '+'										{ PLUS }						| '-'							{ MINUS }
	| '*'										{ TIMES }						| '/'							{ DIVIDE }
	| '='										{ ASSIGN }					| '@'							{ AT }
	| "=="									{ EQ }							| "!="						{ NEQ }
	| '<'										{ LESS }						| "<="						{ LEQ }
	| '>'										{ GRT }							| ">="						{ GEQ }
	| '['										{ LBRACK }					| ']'							{ RBRACK }
	| ".<"									{ LPANGLE }					| '<'							{ LANGLE }
	| '>'										{ RANGLE }					| '|'							{ SPLIT }
	| '#'										{ SEARCH }					|	'~'							{ RM }
	| "&&"									{ AND }							| "||"						{ OR }
	| '!'										{ NOT }							| "<<"						{ COUT }
	| ">>"									{ CIN }							| "int"						{ INT }
	| "string"							{ STR }							| "boolean"				{ BOOL }
	| "if"									{ IF }							| "else"					{ ELSE }
	| "while"								{ WHILE }						| "return"				{ RETURN }
	| "open"								{ OPEN }						| "close"					{ CLOSE }
	| "break"								{ BREAK }						| "eof"						{ EOF }
	| "void"								{ VOID }						| "std"						{ STD }
	| "true"								{ TRUE }						| "false"					{ FALSE }
	| eof										{ END }					(* do as microC *)
	| digit+ as lit					{ LIT_INT(int_of_string lit) }
	| quote _* quote as lit	{ LIT_STR(lit) }
	| lowLetter (lowLetter | upLetter | digit | '_')* as id				{ ID(id) }
	| _ as char 						{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
		"*/"									{ token lexbuf }
	| _											{ comment lexbuf}