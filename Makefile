TARGET1=php_of_ocaml_Type_Base
TARGET2=php_of_ocaml_Array_And_Tuple
TARGET3=php_of_ocaml_Function
TARGET4=php_of_ocaml_IfThenElse
TARGET11=php_of_ocaml_Test


TEST_FILE1=Type_Base.cmt
TEST_FILE2=Array_And_Tuple.cmt
TEST_FILE3=Function.cmt
TEST_FILE4=IfThenElse.cmt
TEST_FILE11=Test.cmt


all: clean
	# on va créer le dossier qui contient les fichier source si'il existe pas 
	if [ ! -d "PHP_generate" ];then mkdir PHP_generate; fi
	# test les variable 
	ocamlbuild -use-ocamlfind  $(TARGET1).native
	ocamlbuild -use-ocamlfind  $(TARGET1).byte
	# test les liste et les tableaux
	ocamlbuild -use-ocamlfind $(TARGET2).native
	ocamlbuild -use-ocamlfind $(TARGET2).byte
	# fonction 
	ocamlbuild -use-ocamlfind $(TARGET3).native
	ocamlbuild -use-ocamlfind $(TARGET3).byte
	# if .. then .. else ... 
	ocamlbuild -use-ocamlfind $(TARGET4).native
	ocamlbuild -use-ocamlfind $(TARGET4).byte
	# pour le fichier test 
	ocamlbuild -use-ocamlfind $(TARGET11).native
	ocamlbuild -use-ocamlfind $(TARGET11).byte

	cd PHP_generate && ocamlc -bin-annot ../tests/*.ml
	

clean:
	rm -rf PHP_generate/*.php
	rm -rf PHP_generate/*.out
	rm -rf src/*.out
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
	cd tests && ../$(TARGET4).native $(TEST_FILE4)
	cd tests && ../$(TARGET11).native $(TEST_FILE11)
	# supprimer les fichier .native et  .byte
	rm -rf *.native
	rm -rf *.byte
	
.PHONY: all clean test
