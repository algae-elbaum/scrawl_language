all: scrawlc test

scrawlc: parse/* semantics/* scrawlc.ml
	@ocamlbuild -use-menhir -Is parse,semantics scrawlc.native
	@mv scrawlc.native scrawlc

test: parse/* semantics/* tests/* scrawlc.ml
	@ocamlbuild -use-menhir -Is parse,semantics,tests test.native
	@mv test.native test

clean:
	@ocamlbuild -clean && rm scrawlc test 2>/dev/null || true
