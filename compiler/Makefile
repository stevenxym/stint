#
# Makefile for Stint Compiler
#
#

OBJS = ast.cmo sast.cmo typecheck.cmo canner.cmo parser.cmo stint.cmo
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

.PHONY: clean cleanall
clean:
	rm -rf *.cmo *cmi $(OBJ_LEX) $(OBJ_YACC) parser.output
cleanall:
	rm -rf *.cmo *cmi $(OBJ_LEX) $(OBJ_YACC) parser.output stint



ast.cmi:
ast.cmx:
sast.cmi:
sast.cmx:
stint.cmo: scanner.cmo parser.cmi ast.cmo
stint.cmx: scanner.cmx parser.cmx ast.cmx
typecheck.cmo: symbolTable.cmo ast.cmo sast.cmo
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmo parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmo
