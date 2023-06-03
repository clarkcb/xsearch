#!/usr/bin/perl -w
#
# fileutil_test.pl
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

use plsearch::FileUtil;

################################################################################
# get_extension tests
################################################################################
sub test_get_extension_has_txt_extension {
    my $file_name = 'file_name.txt';
    my $ext = plsearch::FileUtil::get_extension($file_name);
    ok($ext eq "txt", "$file_name has extension txt");
}

sub test_get_extension_missing_extension {
    my $file_name = 'file_name.';
    my $ext = plsearch::FileUtil::get_extension($file_name);
    ok($ext eq "", "$file_name has missing extension");
}

sub test_get_extension_no_extension {
    my $file_name = 'file_name';
    my $ext = plsearch::FileUtil::get_extension($file_name);
    ok($ext eq "", "$file_name has no extension");
}

sub test_get_extension_hidden_txt_extension {
    my $file_name = '.file_name.txt';
    my $ext = plsearch::FileUtil::get_extension($file_name);
    ok($ext eq "txt", "$file_name has extension txt");
}

sub test_get_extension_hidden_missing_extension {
    my $file_name = '.file_name.';
    my $ext = plsearch::FileUtil::get_extension($file_name);
    ok($ext eq "", "$file_name has missing extension");
}

sub test_get_extension_hidden_no_extension {
    my $file_name = '.file_name';
    my $ext = plsearch::FileUtil::get_extension($file_name);
    ok($ext eq "", "$file_name has no extension");
}

################################################################################
# is_dot_dir tests
################################################################################
sub test_is_dot_dir_single_dot {
    my $file_name = '.';
    my $ok = plsearch::FileUtil::is_dot_dir($file_name);
    ok($ok > 0, "$file_name is dot dir");
}

sub test_is_dot_dir_double_dot {
    my $file_name = '..';
    my $ok = plsearch::FileUtil::is_dot_dir($file_name);
    ok($ok > 0, "$file_name is dot dir");
}

sub test_is_dot_dir_non_dot_dir {
    my $file_name = '.git';
    my $ok = plsearch::FileUtil::is_dot_dir($file_name);
    ok($ok == 0, "$file_name is not dot dir");
}

################################################################################
# is_hidden tests
################################################################################
sub test_is_hidden_hidden_file {
    my $file_name = '.file_name.txt';
    my $ok = plsearch::FileUtil::is_hidden($file_name);
    ok($ok > 0, "$file_name is hidden file");
}

sub test_is_hidden_not_hidden_file {
    my $file_name = 'file_name.txt';
    my $ok = plsearch::FileUtil::is_hidden($file_name);
    ok($ok == 0, "$file_name is not hidden file");
}

sub test_is_hidden_single_dot {
    my $file_name = '.';
    my $ok = plsearch::FileUtil::is_hidden($file_name);
    ok($ok == 0, "$file_name is not hidden file");
}

sub test_is_hidden_double_dot {
    my $file_name = '..';
    my $ok = plsearch::FileUtil::is_hidden($file_name);
    ok($ok == 0, "$file_name is not hidden file");
}

################################################################################
# main
################################################################################
sub main {
    test_get_extension_has_txt_extension();
    test_get_extension_missing_extension();
    test_get_extension_no_extension();
    test_get_extension_hidden_txt_extension();
    test_get_extension_hidden_missing_extension();
    test_get_extension_hidden_no_extension();
    test_is_dot_dir_single_dot();
    test_is_dot_dir_double_dot();
    test_is_dot_dir_non_dot_dir();
    test_is_hidden_hidden_file();
    test_is_hidden_not_hidden_file();
    test_is_hidden_single_dot();
    test_is_hidden_double_dot();
}

main();
