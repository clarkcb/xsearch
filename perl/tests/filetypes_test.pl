#!/usr/bin/perl -w
#
# filetypes_test.pl
#
#
use strict;
use warnings;

use Test::Simple tests => 8;

BEGIN {
  use lib "$ENV{HOME}/src/git/xsearch/perl";
}

use plsearch::FileType;
use plsearch::FileTypes;

my $filetypes = new plsearch::FileTypes();

sub test_getfiletype_archive_file {
    my $filename = 'archive.zip';
    my $ok = $filetypes->is_archive($filename);
    ok($ok > 0, "$filename is archive file");
    my $type = $filetypes->get_filetype($filename);
    ok($type eq plsearch::FileType->ARCHIVE, "FileType of $filename is $type");
}

sub test_getfiletype_binary_file {
    my $filename = 'binary.exe';
    my $ok = $filetypes->is_binary($filename);
    ok($ok > 0, "$filename is binary file");
    my $type = $filetypes->get_filetype($filename);
    ok($type eq plsearch::FileType->BINARY, "FileType of $filename is $type");
}

sub test_getfiletype_text_file {
    my $filename = 'text.txt';
    my $ok = $filetypes->is_text($filename);
    ok($ok > 0, "$filename is text file");
    my $type = $filetypes->get_filetype($filename);
    ok($type eq plsearch::FileType->TEXT, "FileType of $filename is $type");
}

sub test_getfiletype_unknown_file {
    my $filename = 'unknown.xyz';
    my $ok = $filetypes->is_unknown($filename);
    ok($ok > 0, "$filename is unknown file");
    my $type = $filetypes->get_filetype($filename);
    ok($type eq plsearch::FileType->UNKNOWN, "FileType of $filename is $type");
}

sub main {
    test_getfiletype_archive_file();
    test_getfiletype_binary_file();
    test_getfiletype_text_file();
    test_getfiletype_unknown_file();
}

main();
