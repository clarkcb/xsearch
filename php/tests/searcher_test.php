<?php

class SearcherTest extends PHPUnit_Framework_TestCase
{
    private function get_settings()
    {
        $settings = new SearchSettings();
        $settings->startpath = '.';
        array_push($settings->searchpatterns, "Searcher");
        return $settings;
    }

    private function get_test_file()
    {
        $HOME = getenv('HOME');
        return FileUtil::expand_user_home_path(Config::SHAREDPATH . '/testFiles/testFile2.txt');
    }

    ################################################################################
    # is_search_dir tests
    ################################################################################
    public function test_is_search_dir_no_patterns()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $dir = 'plsearch';
        $this->assertTrue($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_matches_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_dirpatterns, 'plsearch');
        $searcher = new Searcher($settings);
        $dir = 'plsearch';
        $this->assertTrue($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_no_match_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_dirpatterns, 'plsearch');
        $searcher = new Searcher($settings);
        $dir = 'pysearch';
        $this->assertFalse($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_matches_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_dirpatterns, 'pysearch');
        $searcher = new Searcher($settings);
        $dir = 'pysearch';
        $this->assertFalse($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_no_match_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_dirpatterns, 'pysearch');
        $searcher = new Searcher($settings);
        $dir = 'plsearch';
        $this->assertTrue($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_single_dot()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $dir = '.';
        $this->assertTrue($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_double_dot()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $dir = '..';
        $this->assertTrue($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_hidden_dir()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $dir = '.git';
        $this->assertFalse($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_hidden_dir_include_hidden()
    {
        $settings = $this->get_settings();
        $settings->excludehidden = 0;
        $searcher = new Searcher($settings);
        $dir = '.git';
        $this->assertTrue($searcher->is_search_dir($dir));
    }

    ################################################################################
    # is_search_file tests
    ################################################################################
    public function test_is_search_file_matches_by_default()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($searcher->is_search_file($file));
    }

    public function test_is_search_file_matches_in_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->in_extensions, 'pm');
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($searcher->is_search_file($file));
    }

    public function test_is_search_file_no_match_in_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->in_extensions, 'pl');
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($searcher->is_search_file($file));
    }

    public function test_is_search_file_matches_out_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->out_extensions, 'pm');
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($searcher->is_search_file($file));
    }

    public function test_is_search_file_no_match_out_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->out_extensions, 'py');
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($searcher->is_search_file($file));
    }

    public function test_is_search_file_matches_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_filepatterns, 'Search');
        $searcher = new Searcher($settings);
        $file = 'Searcher.pm';
        $this->assertTrue($searcher->is_search_file($file));
    }

    public function test_is_search_file_no_match_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_filepatterns, 'Search');
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($searcher->is_search_file($file));
    }

    public function test_is_search_file_matches_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_filepatterns, 'Search');
        $searcher = new Searcher($settings);
        $file = 'Searcher.pm';
        $this->assertFalse($searcher->is_search_file($file));
    }

    public function test_is_search_file_no_match_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_filepatterns, 'Search');
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($searcher->is_search_file($file));
    }

    ################################################################################
    # is__archive_search_file tests
    ################################################################################
    public function test_is_archive_search_file_matches_by_default()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertTrue($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_matches_in_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->in_archiveextensions, 'zip');
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertTrue($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_no_match_in_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->in_archiveextensions, 'gz');
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertFalse($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_matches_out_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->out_archiveextensions, 'zip');
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertFalse($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_no_match_out_extension()
    {
        $settings = $this->get_settings();
        array_push($settings->out_archiveextensions, 'gz');
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertTrue($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_matches_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_archivefilepatterns, 'arch');
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertTrue($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_no_match_in_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->in_archivefilepatterns, 'archives');
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertFalse($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_matches_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_archivefilepatterns, 'arch');
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertFalse($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_no_match_out_pattern()
    {
        $settings = $this->get_settings();
        array_push($settings->out_archivefilepatterns, 'archives');
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertTrue($searcher->is_archive_search_file($file));
    }

    ################################################################################
    # filter_file tests
    ################################################################################
    public function test_filter_file_matches_by_default()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($searcher->filter_file($file));
    }

    public function test_filter_file_is_search_file()
    {
        $settings = $this->get_settings();
        array_push($settings->in_extensions, 'pm');
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($searcher->filter_file($file));
    }

    public function test_filter_file_not_is_search_file()
    {
        $settings = $this->get_settings();
        array_push($settings->in_extensions, 'pl');
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($searcher->filter_file($file));
    }

    public function test_filter_file_is_hidden_file()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $file = '.gitignore';
        $this->assertFalse($searcher->filter_file($file));
    }

    public function test_filter_file_hidden_includehidden()
    {
        $settings = $this->get_settings();
        $settings->excludehidden = 0;
        $searcher = new Searcher($settings);
        $file = '.gitignore';
        $this->assertTrue($searcher->filter_file($file));
    }

    public function test_filter_file_archive_no_searcharchives()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
        $this->assertFalse($searcher->filter_file($file));
    }

    public function test_filter_file_archive_searcharchives()
    {
        $settings = $this->get_settings();
        $settings->searcharchives = 1;
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
        $this->assertTrue($searcher->filter_file($file));
    }

    public function test_filter_file_archive_archivesonly()
    {
        $settings = $this->get_settings();
        $settings->archivesonly = 1;
        $settings->searcharchives = 1;
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
        $this->assertTrue($searcher->filter_file($file));
    }

    public function test_filter_file_nonarchive_archivesonly()
    {
        $settings = $this->get_settings();
        $settings->archivesonly = 1;
        $settings->searcharchives = 1;
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
        $this->assertFalse($searcher->filter_file($file));
    }

    ################################################################################
    # search_lines tests
    ################################################################################
    public function test_search_lines()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $testfile = $this->get_test_file();
        $contents = file_get_contents($testfile);
        $results = $searcher->search_multiline_string($contents);
        $this->assertEquals(count($results), 2);

        $firstResult = $results[0];
        $this->assertEquals($firstResult->linenum, 23);
        $this->assertEquals($firstResult->match_start_index, 3);
        $this->assertEquals($firstResult->match_end_index, 11);

        $secondResult = $results[1];
        $this->assertEquals($secondResult->linenum, 29);
        $this->assertEquals($secondResult->match_start_index, 24);
        $this->assertEquals($secondResult->match_end_index, 32);
    }

    ################################################################################
    # search_multiline_string tests
    ################################################################################
    public function test_search_multiline_string()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $testfile = $this->get_test_file();
        $lines = file($testfile);
        $results = $searcher->search_lines($lines);
        $this->assertEquals(count($results), 2);

        $firstResult = $results[0];
        $this->assertEquals($firstResult->linenum, 23);
        $this->assertEquals($firstResult->match_start_index, 3);
        $this->assertEquals($firstResult->match_end_index, 11);

        $secondResult = $results[1];
        $this->assertEquals($secondResult->linenum, 29);
        $this->assertEquals($secondResult->match_start_index, 24);
        $this->assertEquals($secondResult->match_end_index, 32);
    }
}
