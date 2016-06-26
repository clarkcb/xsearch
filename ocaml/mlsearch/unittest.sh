#!/bin/bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
TESTS=$( find "$DIR" -name "*test.native" | grep -v "_build" )

for t in $TESTS; do
	echo $t
	$t
done
