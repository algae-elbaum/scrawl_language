all: scrawlc test

scrawlc: parse/* scrawlc.ml
	@ocamlbuild -use-menhir -I parse scrawlc.native
	@mv scrawlc.native scrawlc

test: parse/* tests/*
	@ocamlbuild -use-menhir -Is parse,tests test.native
	@mv test.native test

clean:
	@ocamlbuild -clean && rm scrawlc test 2>/dev/null || true
