all: scrawlc test

scrawlc: parse/lexer.mll parse/lex.ml \
         parse/parser.mly parse/parse.ml \
         scrawlc.ml
	@ocamlbuild -I parse scrawlc.native
	@mv scrawlc.native scrawlc

test: parse/lexer.mll parse/lex.ml parse/lex_tests.ml \
      parse/parser.mly parse/parse.ml parse/parse_tests.ml \
      test.ml
	@ocamlbuild -I parse test.native
	@mv test.native test

clean:
	@ocamlbuild -clean && rm scrawlc test 2>/dev/null || true
