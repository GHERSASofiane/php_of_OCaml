
OCAMLC = ocamlc
OCAML = ./

all: Inscription.ml
	$(OCAMLC) str.cma Inscription.ml -o Inscription
	./Inscription
clean:
	rm -r *.cmi *.cmo Inscription

disp:
	$(OCAML)display te.csv
