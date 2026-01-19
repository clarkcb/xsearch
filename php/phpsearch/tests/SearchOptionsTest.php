<?php

declare(strict_types=1);

use phpsearch\SearchException;
use phpsearch\SearchOptions;
use phpsearch\SearchSettings;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

class SearchOptionsTest extends TestCase
{
    public function test_no_args()
    {
        $search_options = new SearchOptions();
        try {
            $settings = $search_options->settings_from_args([]);
        } catch (SearchException $e) {
            $this->fail($e->getMessage());
        }
        $this->assertFalse($settings->archives_only);
        $this->assertTrue($settings->colorize);
        $this->assertFalse($settings->debug);
        $this->assertFalse($settings->first_match);
        $this->assertFalse($settings->follow_symlinks);
        $this->assertFalse($settings->include_hidden);
        $this->assertEquals(0, $settings->lines_after);
        $this->assertEquals(0, $settings->lines_before);
        $this->assertFalse($settings->print_dirs);
        $this->assertFalse($settings->print_files);
        $this->assertFalse($settings->print_lines);
        $this->assertEquals(150, $settings->max_line_length);
        $this->assertFalse($settings->multi_line_search);
        $this->assertTrue($settings->print_results);
        $this->assertFalse($settings->print_usage);
        $this->assertFalse($settings->print_version);
        $this->assertTrue($settings->recursive);
        $this->assertFalse($settings->search_archives);
        $this->assertFalse($settings->unique_lines);
        $this->assertFalse($settings->verbose);
    }

    public function test_valid_args()
    {
        $search_options = new SearchOptions();
        $args = ['-x', 'php,py', '-s', 'Search', '.'];
        try {
            $settings = $search_options->settings_from_args($args);
        } catch (SearchException $e) {
            $this->fail($e->getMessage());
        }
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('php', $settings->in_extensions));
        $this->assertTrue(in_array('py', $settings->in_extensions));
        $this->assertCount(1, $settings->search_patterns);
        $this->assertEquals('Search', $settings->search_patterns[0]);
        $this->assertCount(1, $settings->paths);
        $this->assertEquals('.', $settings->paths[0]);
    }

    public function test_archives_only_arg()
    {
        $search_options = new SearchOptions();
        $args = ['--archivesonly'];
        $settings = $search_options->settings_from_args($args);
        $this->assertTrue($settings->archives_only);
        $this->assertTrue($settings->search_archives);
    }

    public function test_debug_arg()
    {
        $search_options = new SearchOptions();
        $args = ['--debug'];
        $settings = $search_options->settings_from_args($args);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
    }

    public function test_missing_arg()
    {
        $search_options = new SearchOptions();
        $this->expectException(SearchException::class);
        $args = ['-x', 'php,py', '-s', 'Search', '.', '-D'];
        $search_options->settings_from_args($args);
    }

    public function test_invalid_arg()
    {
        $search_options = new SearchOptions();
        $this->expectException(SearchException::class);
        $args = ['-x', 'php,py', '-s', 'Search', '.', '-Q'];
        $search_options->settings_from_args($args);
    }

    public function test_settings_from_json()
    {
        $settings = new SearchSettings();
        $json = <<<"END_JSON"
{
  "path": "~/src/xsearch/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "searchpattern": "Searcher",
  "linesbefore": 2,
  "linesafter": 2,
  "debug": true,
  "allmatches": false,
  "followsymlinks": true,
  "includehidden": true
}
END_JSON;
        $search_options = new SearchOptions();
        $search_options->update_settings_from_json($settings, $json);
        $this->assertCount(1, $settings->paths);
        $this->assertTrue(in_array('~/src/xsearch/', $settings->paths));
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('js', $settings->in_extensions));
        $this->assertTrue(in_array('ts', $settings->in_extensions));
        $this->assertCount(1, $settings->out_dir_patterns);
        $this->assertEquals('node_module', $settings->out_dir_patterns[0]);
        $this->assertCount(1, $settings->out_file_patterns);
        $this->assertEquals('temp', $settings->out_file_patterns[0]);
        $this->assertCount(1, $settings->search_patterns);
        $this->assertEquals('Searcher', $settings->search_patterns[0]);
        $this->assertEquals(2, $settings->lines_before);
        $this->assertEquals(2, $settings->lines_after);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
        $this->assertTrue($settings->first_match);
        $this->assertTrue($settings->follow_symlinks);
        $this->assertTrue($settings->include_hidden);
    }
}
