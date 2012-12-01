#
# Makefile for Stint Compiler
#
#

OBJS = scanner.cmo parser.cmo
stint: $(OBJS)
	ocamlc -o stint $(OBJS)

#parser.ml parser.mli : parser.mly
#	ocamlyacc parser.mly

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
	rm -rf parser.mli parser.ml scanner.ml



parser.cmo: ast.cmi parser.cmi
parser.cmx: ast.cmi parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
ast.cmi:
parser.cmi: ast.cmi
