<?php

declare(strict_types=1);

use phpsearch\SearchSettings;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

class SearchSettingsTest extends TestCase
{
    public function test_default_settings()
    {
        $settings = new SearchSettings();
        $this->assertFalse($settings->archives_only);
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

    public function test_add_single_extension()
    {
        $settings = new SearchSettings();
        $settings->add_exts('php', $settings->in_extensions);
        $this->assertCount(1, $settings->in_extensions);
        $this->assertEquals('php', $settings->in_extensions[0]);
    }

    public function test_add_comma_delimited_extensions()
    {
        $settings = new SearchSettings();
        $settings->add_exts('php,py', $settings->in_extensions);
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('php', $settings->in_extensions));
        $this->assertTrue(in_array('py', $settings->in_extensions));
    }

    public function test_add_extensions_array()
    {
        $settings = new SearchSettings();
        $settings->add_exts(['php','py'], $settings->in_extensions);
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('php', $settings->in_extensions));
        $this->assertTrue(in_array('py', $settings->in_extensions));
    }

    public function test_add_patterns_string()
    {
        $settings = new SearchSettings();
        $settings->add_patterns('Searcher', $settings->search_patterns);
        $this->assertCount(1, $settings->search_patterns);
        $this->assertTrue(in_array('Searcher', $settings->search_patterns));
    }

    public function test_add_patterns_array()
    {
        $settings = new SearchSettings();
        $settings->add_patterns(['Searcher'], $settings->search_patterns);
        $this->assertCount(1, $settings->search_patterns);
        $this->assertTrue(in_array('Searcher', $settings->search_patterns));
    }
}
