#!/bin/sh

# the mlsearch executable
corebuild -package ounit -package re2 -package xml-light -I src mlsearch.native

# unit tests
corebuild -package ounit -I src -I tests fileutiltest.native
corebuild -package ounit -package re2 -package xml-light -I src -I tests filetypestest.native
