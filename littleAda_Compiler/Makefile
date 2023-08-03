all: usage

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

lexer.ml: lexer.mll
	ocamllex $<

test_decoupe: lexer.cmo test_decoupe.cmo
	ocamlc -o $@ $^

parser.ml: parser.mly
	ocamlyacc -v $<

parser.cmo: ast.cmi parser.cmi

parser.cmi: parser.mli ast.cmo
	ocamlc -c $<

test_parser: ast.cmo parser.cmo lexer.cmo test_parser.cmo
	ocamlc -o $@ $^

usage:
	@echo "Taper make  make test_parser"


ast.cmo: ast.cmi
test_decoupe.cmo : lexer.cmo
test_parser.cmo : parser.cmi lexer.cmo ast.cmi
