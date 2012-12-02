#
# Makefile for Stint Compiler
#
#

OBJS = scanner.cmo parser.cmo
OBJ_LEX = scanner.ml
OBJ_YACC = parser.mli parser.ml

stint: $(OBJS)
	ocamlc -o stint $(OBJS)

$(OBJ_LEX): scanner.mll
	ocamllex scanner.mll

$(OBJ_YACC): parser.mly
	ocamlyacc -v parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

.PHONY: clean
clean:
	rm -rf stint *.cmo *cmi $(OBJ_LEX) $(OBJ_YACC) parser.output



parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmo parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
ast.cmi:
parser.cmi: ast.cmo
