<?php declare(strict_types=1);

use phpsearch\SearchException;
use phpsearch\SearchOptions;
use phpsearch\SearchSettings;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

class SearchOptionsTest extends TestCase
{
    /**
     * @var SearchOptions
     */
    private SearchOptions $search_options;

    public function __construct()
    {
        parent::__construct();
        $this->search_options = new SearchOptions();
    }

    public function test_no_args()
    {
        try {
            $settings = $this->search_options->settings_from_args([]);
        } catch (SearchException $e) {
            $this->fail($e->getMessage());
        }
        $this->assertFalse($settings->archives_only);
        $this->assertTrue($settings->colorize);
        $this->assertFalse($settings->debug);
        $this->assertTrue($settings->exclude_hidden);
        $this->assertFalse($settings->first_match);
        $this->assertEquals(0, $settings->lines_after);
        $this->assertEquals(0, $settings->lines_before);
        $this->assertFalse($settings->list_dirs);
        $this->assertFalse($settings->list_files);
        $this->assertFalse($settings->list_lines);
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
        $args = ['-x', 'php,py', '-s', 'Search', '.'];
        try {
            $settings = $this->search_options->settings_from_args($args);
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
        $args = ['--archivesonly'];
        $settings = $this->search_options->settings_from_args($args);
        $this->assertTrue($settings->archives_only);
        $this->assertTrue($settings->search_archives);
    }

    public function test_debug_arg()
    {
        $args = ['--debug'];
        $settings = $this->search_options->settings_from_args($args);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
    }

    public function test_missing_arg()
    {
        $this->expectException(SearchException::class);
        $args = ['-x', 'php,py', '-s', 'Search', '.', '-D'];
        $this->search_options->settings_from_args($args);
    }

    public function test_invalid_arg()
    {
        $this->expectException(SearchException::class);
        $args = ['-x', 'php,py', '-s', 'Search', '.', '-Q'];
        $this->search_options->settings_from_args($args);
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
  "includehidden": true
}
END_JSON;
        $this->search_options->settings_from_json($json, $settings);
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
        $this->assertTrue(!$settings->exclude_hidden);
    }
}
