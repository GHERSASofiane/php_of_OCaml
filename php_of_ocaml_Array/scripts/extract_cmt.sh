#! /bin/sh -e

PP=

FILENAME=$(basename -s ".ml" $@)

if [ "$#" -ne 1 ]; then
    echo "Usage : extract_cmt <ml_file>"
    exit 1
fi


$PP ocamlc -bin-annot "$@"
$PP rm -f $FILENAME.cm[io] a.out
