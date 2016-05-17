all: scrawlc test

scrawlc: parse/* scrawlc.ml
	@ocamlbuild -use-menhir -Is parse,semantics scrawlc.native
	@mv scrawlc.native scrawlc

test: parse/* tests/*
	@ocamlbuild -use-menhir -Is parse,semantics,tests test.native
	@mv test.native test

clean:
	@ocamlbuild -clean && rm scrawlc test 2>/dev/null || true
