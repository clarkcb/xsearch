<?php

declare(strict_types=1);

use phpfind\FileUtil;
use phpsearch\Config;
use phpsearch\Searcher;
use phpsearch\SearchException;
use phpsearch\SearchSettings;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

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
        return FileUtil::expand_user_home_path(Config::XSEARCH_SHARED_PATH . '/testFiles/testFile2.txt');
    }

    ################################################################################
    # test_search_multi_line_string
    ################################################################################
    public function test_search_multi_line_string()
    {
        $settings = $this->get_settings();
        try {
            $searcher = new Searcher($settings);
            $test_file = $this->get_test_file();
            $contents = file_get_contents($test_file);
            $results = $searcher->search_multi_line_string($contents);
            $this->assertCount(2, $results);

            $firstResult = $results[0];
            $this->assertEquals(30, $firstResult->line_num);
            $this->assertEquals(3, $firstResult->match_start_index);
            $this->assertEquals(11, $firstResult->match_end_index);

            $secondResult = $results[1];
            $this->assertEquals(36, $secondResult->line_num);
            $this->assertEquals(24, $secondResult->match_start_index);
            $this->assertEquals(32, $secondResult->match_end_index);
        } catch (SearchException $e) {
            $this->fail($e->getMessage());
        }
    }

    ################################################################################
    # test_search_lines
    ################################################################################
    public function test_search_lines()
    {
        $settings = $this->get_settings();
        try {
            $searcher = new Searcher($settings);
            $test_file = $this->get_test_file();
            $lines = file($test_file);
            $results = $searcher->search_lines($lines);
            $this->assertCount(2, $results);

            $firstResult = $results[0];
            $this->assertEquals(30, $firstResult->line_num);
            $this->assertEquals(3, $firstResult->match_start_index);
            $this->assertEquals(11, $firstResult->match_end_index);

            $secondResult = $results[1];
            $this->assertEquals(36, $secondResult->line_num);
            $this->assertEquals(24, $secondResult->match_start_index);
            $this->assertEquals(32, $secondResult->match_end_index);
        } catch (SearchException $e) {
            $this->fail($e->getMessage());
        }

    }
}
