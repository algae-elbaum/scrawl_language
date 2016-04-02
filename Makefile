EXEC = scrawlc

all: exec

exec: lexer.ml
	ocamlopt lexer.ml lex.ml scrawlc.ml -o $(EXEC)
	rm *.cmi *.cmx *.o

lexer.ml: lexer.mll
	ocamllex lexer.mll

# Currently not used
parser.ml: parser.mly
	ocamlyacc parser.mly
    
run: exec
	@./$(EXEC)

clean:
	@rm *.mli *.cmi *.cmx *o scrawlc lexer.ml parser.ml 2>/dev/null || true
