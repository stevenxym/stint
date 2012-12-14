#
# Makefile for Stint Compiler
#
#

OBJS = scanner.cmo parser.cmo				\
	ast.cmo sast.cmo symboltable.cmo typecheck.cmo	\
	translator.cmo stint.cmo
OBJ_LEX = scanner.ml
OBJ_YACC = parser.mli parser.ml

stint: $(OBJS)
	ocamlc -o stint unix.cma $(OBJS)

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



#ast.cmi:
#ast.cmx:
#sast.cmi:
#sast.cmx:
#symboltable.cmo:
#symboltable.cmx:
stint.cmo: scanner.cmo parser.cmi ast.cmo typecheck.cmo translator.cmo
stint.cmx: scanner.cmx parser.cmx ast.cmx typecheck.cmx translator.cmx
typecheck.cmo: symboltable.cmo ast.cmo sast.cmo
typecheck.cmx: symboltable.cmx ast.cmx sast.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmo parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmo
translator.cmo: sast.cmo
translator.cmx: sast.cmx
