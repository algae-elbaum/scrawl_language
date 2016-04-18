all: scrawlc test

scrawlc: parse/lexer.mll parse/lex.ml \
         parse/abstract_syntax.ml \
         parse/parser.mly parse/parse.ml \
         scrawlc.ml
	@ocamlbuild -use-menhir -I parse scrawlc.native
	@mv scrawlc.native scrawlc

test: parse/lexer.mll parse/lex.ml parse/lex_tests.ml \
      parse/abstract_syntax.ml \
      parse/parser.mly parse/parse.ml parse/parse_tests.ml \
      test.ml
	@ocamlbuild -use-menhir -I parse test.native
	@mv test.native test

clean:
	@ocamlbuild -clean && rm scrawlc test 2>/dev/null || true
