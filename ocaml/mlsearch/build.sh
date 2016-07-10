#!/bin/bash

# the mlsearch executable
echo 'corebuild -package ounit -package re2 -package xml-light -I src mlsearch.native'
corebuild -package ounit -package re2 -package xml-light -I src mlsearch.native

# unit tests
cd tests
TESTFILES=$(ls *test.ml)
cd -

for f in $TESTFILES; do
	echo
	filename="${f%.*}"
    echo 'corebuild -package ounit -package re2 -package xml-light -I src -I tests '"$filename"'.native'
    corebuild -package ounit -package re2 -package xml-light -I src -I tests "$filename".native
done
