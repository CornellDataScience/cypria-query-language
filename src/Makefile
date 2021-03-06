UNITS=interpreter reverse_parser parser variable static_analyzer menhir_parser parse
MLS_WITHOUT_MLIS=ast main
MLS=$(MLS_WITHOUT_MLIS:=.ml) $(UNITS:=.ml)
OBJECTS=$(UNITS:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo)
MLIS=$(UNITS:=.mli)
TEST=test.byte
MAIN=main.byte
PARSE = parse.byte
REPL=repl.byte
OCAMLBUILD=ocamlbuild -no-hygiene -use-ocamlfind -use-menhir -pkgs oUnit,str
PKGS=oUnit,str

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

repl:
	$(OCAMLBUILD) $(REPL) && ./$(REPL)

main: 	
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

buildparse: 
	$(OCAMLBUILD) -use-menhir $(PARSE)

parse:
	$(OCAMLBUILD) $(PARSE) && ./$(PARSE)
	
check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

zip:
	zip cypria.zip *.ml* _tags Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private cypria.zip
