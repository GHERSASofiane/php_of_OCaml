
OCAMLC = ocamlc
OCAML = ./

all: CSV.mli CSV.ml
	$(OCAMLC) -c CSV.mli
	$(OCAMLC) str.cma CSV.ml -o display

clean:
	rm -r *.cmi *.cmo display

test:
	$(OCAML)display te.csv
