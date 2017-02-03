TARGET1=php_of_ocaml_Type_Base
TARGET2=php_of_ocaml_Array_And_Tuple
TARGET3=php_of_ocaml_Test


TEST_FILE1=Type_Base.cmt
TEST_FILE2=Array_And_Tuple.cmt
TEST_FILE3=Test.cmt


all: clean

	# test les variable 
	ocamlbuild -use-ocamlfind  $(TARGET1).native
	ocamlbuild -use-ocamlfind  $(TARGET1).byte
	# test les liste et les tableaux
	ocamlbuild -use-ocamlfind $(TARGET2).native
	ocamlbuild -use-ocamlfind $(TARGET2).byte
	# pour le fichier test 
	ocamlbuild -use-ocamlfind $(TARGET3).native
	ocamlbuild -use-ocamlfind $(TARGET3).byte

	cd PHP_generate && ocamlc -bin-annot ../tests/*.ml
	

clean:
	rm -rf PHP_generate/*.php
	rm -rf PHP_generate/*.out
	rm -rf tests/*.cmt
	rm -rf tests/*.cmi
	rm -rf tests/*.cmo
	# supprimer les fichier .native et  .byte
	rm -rf *.native
	rm -rf *.byte

test: all
	cd tests && ../$(TARGET1).native $(TEST_FILE1)
	cd tests && ../$(TARGET2).native $(TEST_FILE2)
	cd tests && ../$(TARGET3).native $(TEST_FILE3)
	# supprimer les fichier .native et  .byte
	rm -rf *.native
	rm -rf *.byte
	
.PHONY: all clean test
