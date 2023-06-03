#!/usr/bin/perl -w
#
# file_types_test.pl
#
#
use strict;
use warnings;

use Cwd 'abs_path';
use File::Basename;

my $lib_path;

BEGIN {
    $lib_path = dirname(dirname(abs_path($0))) . '/lib';
    # print "lib_path: $lib_path\n";
    unshift @INC, $lib_path;
}

use Test::Simple tests => 13;

use plsearch::FileType;
use plsearch::FileTypes;

my $file_types = new plsearch::FileTypes();

sub test_get_file_type_archive_file {
    my $file_name = 'archive.zip';
    my $ok = $file_types->is_archive($file_name);
    ok($ok > 0, "$file_name is archive file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plsearch::FileType->ARCHIVE, "FileType of $file_name is $type");
}

sub test_get_file_type_binary_file {
    my $file_name = 'binary.exe';
    my $ok = $file_types->is_binary($file_name);
    ok($ok > 0, "$file_name is binary file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plsearch::FileType->BINARY, "FileType of $file_name is $type");
}

sub test_get_file_type_code_file {
    my $file_name = 'code.pl';
    my $ok = $file_types->is_code($file_name);
    ok($ok > 0, "$file_name is code file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plsearch::FileType->CODE, "FileType of $file_name is $type");
}

sub test_get_file_type_text_file {
    my $file_name = 'text.txt';
    my $ok = $file_types->is_text($file_name);
    ok($ok > 0, "$file_name is text file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plsearch::FileType->TEXT, "FileType of $file_name is $type");
}

sub test_get_file_type_xml_file {
    my $file_name = 'markup.xml';
    my $ok = $file_types->is_xml($file_name);
    ok($ok > 0, "$file_name is xml file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plsearch::FileType->XML, "FileType of $file_name is $type");
}

sub test_get_file_type_searchable_file {
    my $file_name = 'archive.zip';
    my $ok = $file_types->is_searchable($file_name);
    ok($ok > 0, "$file_name is searchable file");
    # my $type = $file_types->get_file_type($file_name);
    # ok($type eq plsearch::FileType->UNKNOWN, "FileType of $file_name is $type");
}

sub test_get_file_type_unknown_file {
    my $file_name = 'unknown.xyz';
    my $ok = $file_types->is_unknown($file_name);
    ok($ok > 0, "$file_name is unknown file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plsearch::FileType->UNKNOWN, "FileType of $file_name is $type");
}

sub main {
    test_get_file_type_archive_file();
    test_get_file_type_binary_file();
    test_get_file_type_code_file();
    test_get_file_type_text_file();
    test_get_file_type_xml_file();
    test_get_file_type_searchable_file();
    test_get_file_type_unknown_file();
}

main();
