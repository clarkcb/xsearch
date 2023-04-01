# Before 'make install' is performed this script should be runnable with
# 'make test'. After 'make install' it should work as 'perl plsearch.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use strict;
use warnings;

use Test::Simple tests => 6;

use plsearch::FileType;
use plsearch::FileTypes;

ok(1); # If we made it this far, we're ok.

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

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

sub test_getfiletype_code_file {
    my $filename = 'code.pl';
    my $ok = $filetypes->is_code($filename);
    ok($ok > 0, "$filename is code file");
    my $type = $filetypes->get_filetype($filename);
    ok($type eq plsearch::FileType->CODE, "FileType of $filename is $type");
}

sub test_getfiletype_text_file {
    my $filename = 'text.txt';
    my $ok = $filetypes->is_text($filename);
    ok($ok > 0, "$filename is text file");
    my $type = $filetypes->get_filetype($filename);
    ok($type eq plsearch::FileType->TEXT, "FileType of $filename is $type");
}

sub test_getfiletype_xml_file {
    my $filename = 'markup.xml';
    my $ok = $filetypes->is_xml($filename);
    ok($ok > 0, "$filename is xml file");
    my $type = $filetypes->get_filetype($filename);
    ok($type eq plsearch::FileType->XML, "FileType of $filename is $type");
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
    test_getfiletype_code_file();
    test_getfiletype_text_file();
    test_getfiletype_xml_file();
    test_getfiletype_unknown_file();
}

main();
