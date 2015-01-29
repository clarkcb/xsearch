#!/usr/bin/perl -w
#
# fileutil_test.pl
#
#
use strict;
use warnings;

use Test::Simple tests => 25;

use lib '/Users/cary/src/git/xsearch/perl';

use plsearch::FileUtil;

################################################################################
# get_extension tests
################################################################################
sub test_get_extension_has_txt_extension {
    my $filename = 'filename.txt';
    my $ext = plsearch::FileUtil::get_extension($filename);
    ok($ext eq "txt", "$filename has extension txt");
}

sub test_get_extension_missing_extension {
    my $filename = 'filename.';
    my $ext = plsearch::FileUtil::get_extension($filename);
    ok($ext eq "", "$filename has missing extension");
}

sub test_get_extension_no_extension {
    my $filename = 'filename';
    my $ext = plsearch::FileUtil::get_extension($filename);
    ok($ext eq "", "$filename has no extension");
}

sub test_get_extension_hidden_txt_extension {
    my $filename = '.filename.txt';
    my $ext = plsearch::FileUtil::get_extension($filename);
    ok($ext eq "txt", "$filename has extension txt");
}

sub test_get_extension_hidden_missing_extension {
    my $filename = '.filename.';
    my $ext = plsearch::FileUtil::get_extension($filename);
    ok($ext eq "", "$filename has missing extension");
}

sub test_get_extension_hidden_no_extension {
    my $filename = '.filename';
    my $ext = plsearch::FileUtil::get_extension($filename);
    ok($ext eq "", "$filename has no extension");
}

################################################################################
# is_dot_dir tests
################################################################################
sub test_is_dot_dir_single_dot {
    my $filename = '.';
    my $ok = plsearch::FileUtil::is_dot_dir($filename);
    ok($ok > 0, "$filename is dot dir");
}

sub test_is_dot_dir_double_dot {
    my $filename = '..';
    my $ok = plsearch::FileUtil::is_dot_dir($filename);
    ok($ok > 0, "$filename is dot dir");
}

sub test_is_dot_dir_non_dot_dir {
    my $filename = '.git';
    my $ok = plsearch::FileUtil::is_dot_dir($filename);
    ok($ok == 0, "$filename is not dot dir");
}

################################################################################
# is_hidden tests
################################################################################
sub test_is_hidden_hidden_file {
    my $filename = '.filename.txt';
    my $ok = plsearch::FileUtil::is_hidden($filename);
    ok($ok > 0, "$filename is hidden file");
}

sub test_is_hidden_not_hidden_file {
    my $filename = 'filename.txt';
    my $ok = plsearch::FileUtil::is_hidden($filename);
    ok($ok == 0, "$filename is not hidden file");
}

sub test_is_hidden_single_dot {
    my $filename = '.';
    my $ok = plsearch::FileUtil::is_hidden($filename);
    ok($ok == 0, "$filename is not hidden file");
}

sub test_is_hidden_double_dot {
    my $filename = '..';
    my $ok = plsearch::FileUtil::is_hidden($filename);
    ok($ok == 0, "$filename is not hidden file");
}

################################################################################
# split_path tests
################################################################################
sub test_split_path_path_with_forward_slashes {
    my $filepath = '~/path/to/filename.txt';
    my $elems = plsearch::FileUtil::split_path($filepath);
    ok(scalar @{$elems} == 4, "$filepath has 4 path elems");
    ok($elems->[0] eq '~', "$filepath has '~' as first elem");
    ok($elems->[1] eq 'path', "$filepath has 'path' as second elem");
    ok($elems->[2] eq 'to', "$filepath has 'to' as third elem");
    ok($elems->[3] eq 'filename.txt',
        "$filepath has 'filename.txt' as last elem");
}

sub test_split_path_path_with_back_slashes {
    my $filepath = 'C:\\path\\to\\filename.txt';
    my $elems = plsearch::FileUtil::split_path($filepath);
    ok(scalar @{$elems} == 4, "$filepath has 4 path elems");
    ok($elems->[0] eq 'C:', "$filepath has 'C:' as first elem");
    ok($elems->[1] eq 'path', "$filepath has 'path' as second elem");
    ok($elems->[2] eq 'to', "$filepath has 'to' as third elem");
    ok($elems->[3] eq 'filename.txt',
        "$filepath has 'filename.txt' as last elem");
}

sub test_split_path_path_without_slashes {
    my $filepath = 'filename.txt';
    my $elems = plsearch::FileUtil::split_path($filepath);
    ok(scalar @{$elems} == 1, "$filepath has 1 path elem");
    ok($elems->[0] eq 'filename.txt',
        "$filepath has 'filename.txt' as first elem");
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
    test_split_path_path_with_forward_slashes();
    test_split_path_path_with_back_slashes();
    test_split_path_path_without_slashes();
}

main();
