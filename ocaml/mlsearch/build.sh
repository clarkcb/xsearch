#!/bin/bash




build_src () {
	# the mlsearch executable
	echo 'corebuild -package ounit -package re2 -package xml-light -I src mlsearch.native'
	corebuild -package ounit -package re2 -package xml-light -I src mlsearch.native
}

build_tests () {
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
}


build_src

build_tests
