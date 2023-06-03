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
    test_get_file_type_unknown_file();
}

main();
