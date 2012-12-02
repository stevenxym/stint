#
# Makefile for Stint Compiler
#
#

OBJS = scanner.cmo parser.cmo
stint: $(OBJS)
	ocamlc -o stint $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc -v parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

.PHONY: clean
clean:
	rm -rf stint *.cmo *cmi parser.mli parser.ml scanner.ml



parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmo parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
ast.cmi:
parser.cmi: ast.cmo
