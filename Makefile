TARGET=php_of_ocaml
TEST_FILE=let-binding.cmt


all:
	ocamlbuild -use-ocamlfind $(TARGET).native
	ocamlbuild -use-ocamlfind $(TARGET).byte

clean:
	ocamlbuild -clean
	rm -rf tests/*.php

tests: all
	$(MAKE) -C tests

.PHONY: all clean test
