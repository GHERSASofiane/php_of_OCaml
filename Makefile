TARGET=php_of_ocaml
TEST_FILE=let-binding.cmt


all:
	ocamlbuild -use-ocamlfind $(TARGET).native
	ocamlbuild -use-ocamlfind $(TARGET).byte

clean:
	ocamlbuild -clean
	rm -rf tests/*.php
	rm -rf tests/*.cmt
	rm -rf tests/*.cmi
	rm -rf tests/*.cmo
	rm -rf tests/*.out

test: all
	cd tests && ../$(TARGET).native $(TEST_FILE)

.PHONY: all clean test