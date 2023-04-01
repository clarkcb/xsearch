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
        $this->assertFalse($this->settings->archivesonly);
        $this->assertFalse($this->settings->debug);
        $this->assertTrue($this->settings->excludehidden);
        $this->assertFalse($this->settings->firstmatch);
        $this->assertEquals(0, $this->settings->linesafter);
        $this->assertEquals(0, $this->settings->linesbefore);
        $this->assertFalse($this->settings->listdirs);
        $this->assertFalse($this->settings->listfiles);
        $this->assertFalse($this->settings->listlines);
        $this->assertEquals(150, $this->settings->maxlinelength);
        $this->assertFalse($this->settings->multilinesearch);
        $this->assertTrue($this->settings->printresults);
        $this->assertFalse($this->settings->printusage);
        $this->assertFalse($this->settings->printversion);
        $this->assertTrue($this->settings->recursive);
        $this->assertFalse($this->settings->searcharchives);
        $this->assertFalse($this->settings->uniquelines);
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
        $this->settings->add_patterns('Searcher', $this->settings->searchpatterns);
        $this->assertCount(1, $this->settings->searchpatterns);
        $this->assertTrue(in_array('Searcher', $this->settings->searchpatterns));
    }

    public function test_add_patterns_array()
    {
        $this->settings->add_patterns(['Searcher'], $this->settings->searchpatterns);
        $this->assertCount(1, $this->settings->searchpatterns);
        $this->assertTrue(in_array('Searcher', $this->settings->searchpatterns));
    }
}
