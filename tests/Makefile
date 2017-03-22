FILES=Array_And_Tuple.ml Function.ml IfThenElse.ml Test.ml Type_Base.ml Function_Print.ml unary_operator.ml while_exp.ml for_exp.ml construct_exp.ml csv.ml
CC=../php_of_ocaml.native

OUTPUT=$(FILES:.ml=.php)

all: $(OUTPUT)

%.cmt:%.ml
	../scripts/extract_cmt.sh $<

%.php:%.cmt
	$(CC) $< -o $@
