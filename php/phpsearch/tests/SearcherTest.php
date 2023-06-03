<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpsearch\Config;
use phpsearch\FileUtil;
use phpsearch\Searcher;
use phpsearch\SearchSettings;

class SearcherTest extends TestCase
{
    private function get_settings(): SearchSettings
    {
        $settings = new SearchSettings();
        $settings->paths = ['.'];
        $settings->search_patterns[] = "Searcher";
        return $settings;
    }

    private function get_test_file(): string
    {
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
        $settings->in_dir_patterns[] = 'plsearch';
        $searcher = new Searcher($settings);
        $dir = 'plsearch';
        $this->assertTrue($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_no_match_in_pattern()
    {
        $settings = $this->get_settings();
        $settings->in_dir_patterns[] = 'plsearch';
        $searcher = new Searcher($settings);
        $dir = 'pysearch';
        $this->assertFalse($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_matches_out_pattern()
    {
        $settings = $this->get_settings();
        $settings->out_dir_patterns[] = 'pysearch';
        $searcher = new Searcher($settings);
        $dir = 'pysearch';
        $this->assertFalse($searcher->is_search_dir($dir));
    }

    public function test_is_search_dir_no_match_out_pattern()
    {
        $settings = $this->get_settings();
        $settings->out_dir_patterns[] = 'pysearch';
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
        $settings->exclude_hidden = false;
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
        $settings->in_extensions[] = 'pm';
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($searcher->is_search_file($file));
    }

    public function test_is_search_file_no_match_in_extension()
    {
        $settings = $this->get_settings();
        $settings->in_extensions[] = 'pl';
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($searcher->is_search_file($file));
    }

    public function test_is_search_file_matches_out_extension()
    {
        $settings = $this->get_settings();
        $settings->out_extensions[] = 'pm';
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($searcher->is_search_file($file));
    }

    public function test_is_search_file_no_match_out_extension()
    {
        $settings = $this->get_settings();
        $settings->out_extensions[] = 'py';
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($searcher->is_search_file($file));
    }

    public function test_is_search_file_matches_in_pattern()
    {
        $settings = $this->get_settings();
        $settings->in_file_patterns[] = 'Search';
        $searcher = new Searcher($settings);
        $file = 'Searcher.pm';
        $this->assertTrue($searcher->is_search_file($file));
    }

    public function test_is_search_file_no_match_in_pattern()
    {
        $settings = $this->get_settings();
        $settings->in_file_patterns[] = 'Search';
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($searcher->is_search_file($file));
    }

    public function test_is_search_file_matches_out_pattern()
    {
        $settings = $this->get_settings();
        $settings->out_file_patterns[] = 'Search';
        $searcher = new Searcher($settings);
        $file = 'Searcher.pm';
        $this->assertFalse($searcher->is_search_file($file));
    }

    public function test_is_search_file_no_match_out_pattern()
    {
        $settings = $this->get_settings();
        $settings->out_file_patterns[] = 'Search';
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
        $settings->in_archive_extensions[] = 'zip';
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertTrue($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_no_match_in_extension()
    {
        $settings = $this->get_settings();
        $settings->in_archive_extensions[] = 'gz';
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertFalse($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_matches_out_extension()
    {
        $settings = $this->get_settings();
        $settings->out_archive_extensions[] = 'zip';
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertFalse($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_no_match_out_extension()
    {
        $settings = $this->get_settings();
        $settings->out_archive_extensions[] = 'gz';
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertTrue($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_matches_in_pattern()
    {
        $settings = $this->get_settings();
        $settings->in_archive_file_patterns[] = 'arch';
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertTrue($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_no_match_in_pattern()
    {
        $settings = $this->get_settings();
        $settings->in_archive_file_patterns[] = 'archives';
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertFalse($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_matches_out_pattern()
    {
        $settings = $this->get_settings();
        $settings->out_archive_file_patterns[] = 'arch';
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertFalse($searcher->is_archive_search_file($file));
    }

    public function test_is_archive_search_file_no_match_out_pattern()
    {
        $settings = $this->get_settings();
        $settings->out_archive_file_patterns[] = 'archives';
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
        $settings->in_extensions[] = 'pm';
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertTrue($searcher->filter_file($file));
    }

    public function test_filter_file_not_is_search_file()
    {
        $settings = $this->get_settings();
        $settings->in_extensions[] = 'pl';
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
        $settings->exclude_hidden = false;
        $searcher = new Searcher($settings);
        $file = '.gitignore';
        $this->assertTrue($searcher->filter_file($file));
    }

    public function test_filter_file_archive_no_search_archives()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
        $this->assertFalse($searcher->filter_file($file));
    }

    public function test_filter_file_archive_search_archives()
    {
        $settings = $this->get_settings();
        $settings->search_archives = true;
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        #print "searcher->is_archive_search_file(archive.zip): " . $searcher->is_archive_search_file('archive.zip') . "\n";
        $this->assertTrue($searcher->filter_file($file));
    }

    public function test_filter_file_archive_archives_only()
    {
        $settings = $this->get_settings();
        $settings->archives_only = true;
        $settings->search_archives = true;
        $searcher = new Searcher($settings);
        $file = 'archive.zip';
        $this->assertTrue($searcher->filter_file($file));
    }

    public function test_filter_file_nonarchive_archives_only()
    {
        $settings = $this->get_settings();
        $settings->archives_only = true;
        $settings->search_archives = true;
        $searcher = new Searcher($settings);
        $file = 'FileUtil.pm';
        $this->assertFalse($searcher->filter_file($file));
    }

    ################################################################################
    # search_lines tests
    ################################################################################
    public function test_search_lines()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $test_file = $this->get_test_file();
        $contents = file_get_contents($test_file);
        $results = $searcher->search_multi_line_string($contents);
        $this->assertCount(2, $results);

        $firstResult = $results[0];
        $this->assertEquals(29, $firstResult->linenum);
        $this->assertEquals(3, $firstResult->match_start_index);
        $this->assertEquals(11, $firstResult->match_end_index);

        $secondResult = $results[1];
        $this->assertEquals(35, $secondResult->linenum);
        $this->assertEquals(24, $secondResult->match_start_index);
        $this->assertEquals(32, $secondResult->match_end_index);
    }

    ################################################################################
    # search_multi_line_string tests
    ################################################################################
    public function test_search_multi_line_string()
    {
        $settings = $this->get_settings();
        $searcher = new Searcher($settings);
        $test_file = $this->get_test_file();
        $lines = file($test_file);
        $results = $searcher->search_lines($lines);
        $this->assertCount(2, $results);

        $firstResult = $results[0];
        $this->assertEquals(29, $firstResult->linenum);
        $this->assertEquals(3, $firstResult->match_start_index);
        $this->assertEquals(11, $firstResult->match_end_index);

        $secondResult = $results[1];
        $this->assertEquals(35, $secondResult->linenum);
        $this->assertEquals(24, $secondResult->match_start_index);
        $this->assertEquals(32, $secondResult->match_end_index);
    }
}
