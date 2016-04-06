all: scrawlc test

scrawlc: lex/lexer.mll lex/lex.ml scrawlc.ml
	@ocamlbuild -I lex scrawlc.native
	@mv scrawlc.native scrawlc

test: lex/lexer.mll lex/lex.ml lex/lex_tests.ml test.ml
	@ocamlbuild -I lex test.native
	@mv test.native test

clean:
	@ocamlbuild -clean && rm scrawlc test 2>/dev/null || true
