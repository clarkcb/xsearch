<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpsearch\SearchSettings;

class SearchSettingsTest extends TestCase
{
    /**
     * @var SearchSettings
     */
    private $settings;

    public function __construct()
    {
        parent::__construct();
        $this->settings = new SearchSettings();
    }

    public function test_default_settings()
    {
        $this->assertFalse($this->settings->archives_only);
        $this->assertFalse($this->settings->debug);
        $this->assertTrue($this->settings->exclude_hidden);
        $this->assertFalse($this->settings->first_match);
        $this->assertEquals(0, $this->settings->lines_after);
        $this->assertEquals(0, $this->settings->lines_before);
        $this->assertFalse($this->settings->list_dirs);
        $this->assertFalse($this->settings->list_files);
        $this->assertFalse($this->settings->list_lines);
        $this->assertEquals(150, $this->settings->max_line_length);
        $this->assertFalse($this->settings->multi_line_search);
        $this->assertTrue($this->settings->print_results);
        $this->assertFalse($this->settings->print_usage);
        $this->assertFalse($this->settings->print_version);
        $this->assertTrue($this->settings->recursive);
        $this->assertFalse($this->settings->search_archives);
        $this->assertFalse($this->settings->unique_lines);
        $this->assertFalse($this->settings->verbose);
    }

    public function test_add_single_extension()
    {
        $this->settings->add_exts('php', $this->settings->in_extensions);
        $this->assertCount(1, $this->settings->in_extensions);
        $this->assertEquals('php', $this->settings->in_extensions[0]);
    }

    public function test_add_comma_delimited_extensions()
    {
        $this->settings->add_exts('php,py', $this->settings->in_extensions);
        $this->assertCount(2, $this->settings->in_extensions);
        $this->assertTrue(in_array('php', $this->settings->in_extensions));
        $this->assertTrue(in_array('py', $this->settings->in_extensions));
    }

    public function test_add_extensions_array()
    {
        $this->settings->add_exts(['php','py'], $this->settings->in_extensions);
        $this->assertCount(2, $this->settings->in_extensions);
        $this->assertTrue(in_array('php', $this->settings->in_extensions));
        $this->assertTrue(in_array('py', $this->settings->in_extensions));
    }

    public function test_add_patterns_string()
    {
        $this->settings->add_patterns('Searcher', $this->settings->search_patterns);
        $this->assertCount(1, $this->settings->search_patterns);
        $this->assertTrue(in_array('Searcher', $this->settings->search_patterns));
    }

    public function test_add_patterns_array()
    {
        $this->settings->add_patterns(['Searcher'], $this->settings->search_patterns);
        $this->assertCount(1, $this->settings->search_patterns);
        $this->assertTrue(in_array('Searcher', $this->settings->search_patterns));
    }
}
