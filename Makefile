all: scrawlc test

scrawlc: lexer.cmx lex.cmx scrawlc.cmx
	@ocamlopt lexer.cmx lex.cmx scrawlc.cmx -o scrawlc

test: lexer.cmx lex.cmx lex_tests.cmx test.cmx
	@ocamlopt lexer.cmx lex.cmx lex_tests.cmx test.cmx -o test

scrawlc.cmx: scrawlc.ml
	@ocamlopt -c scrawlc.ml

test.cmx: test.ml
	@ocamlopt -c test.ml

lex_tests.cmx: lex_tests.ml
	@ocamlopt -c lex_tests.ml

lex.cmx: lex.ml
	@ocamlopt -c lex.ml

lexer.cmx: lexer.mll
	@ocamllex lexer.mll >/dev/null
	@ocamlopt -c lexer.ml

# Currently not used
parser.cmx: parser.mly
	@ocamlyacc parser.mly
	@ocamlopt -c parser.ml

    
run: exec
	@./$(EXEC)

clean:
	@rm *.mli *.cmi *.cmx *.o scrawlc test lexer.ml parser.ml 2>/dev/null || true
