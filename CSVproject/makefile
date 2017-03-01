
OCAMLC = ocamlc
OCAML = ./

all: CSV.ml writeCSV.ml searchCSV.ml deleteCSV.ml
	$(OCAMLC) str.cma writeCSV.ml -o writein
	$(OCAMLC) str.cma searchCSV.ml -o searchin
	$(OCAMLC) str.cma deleteCSV.ml -o delete
	$(OCAMLC) str.cma CSV.ml -o display
clean:
	rm -r *.cmi *.cmo display writein searchin delete

disp:
	$(OCAML)display te.csv
