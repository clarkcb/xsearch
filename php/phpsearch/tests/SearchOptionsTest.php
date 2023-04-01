<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpsearch\SearchException;
use phpsearch\SearchOptions;
use phpsearch\SearchSettings;

class SearchOptionsTest extends TestCase
{
    /**
     * @var SearchOptions
     */
    private $searchoptions;

    public function __construct()
    {
        parent::__construct();
        $this->searchoptions = new SearchOptions();
    }

    public function test_no_args()
    {
        $settings = $this->searchoptions->settings_from_args([]);
        $this->assertFalse($settings->archivesonly);
        $this->assertTrue($settings->colorize);
        $this->assertFalse($settings->debug);
        $this->assertTrue($settings->excludehidden);
        $this->assertFalse($settings->firstmatch);
        $this->assertEquals(0, $settings->linesafter);
        $this->assertEquals(0, $settings->linesbefore);
        $this->assertFalse($settings->listdirs);
        $this->assertFalse($settings->listfiles);
        $this->assertFalse($settings->listlines);
        $this->assertEquals(150, $settings->maxlinelength);
        $this->assertFalse($settings->multilinesearch);
        $this->assertTrue($settings->printresults);
        $this->assertFalse($settings->printusage);
        $this->assertFalse($settings->printversion);
        $this->assertTrue($settings->recursive);
        $this->assertFalse($settings->searcharchives);
        $this->assertFalse($settings->uniquelines);
        $this->assertFalse($settings->verbose);
    }

    public function test_valid_args()
    {
        $args = ['-x', 'php,py', '-s', 'Search', '.'];
        $settings = $this->searchoptions->settings_from_args($args);
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('php', $settings->in_extensions));
        $this->assertTrue(in_array('py', $settings->in_extensions));
        $this->assertCount(1, $settings->searchpatterns);
        $this->assertEquals('Search', $settings->searchpatterns[0]);
        $this->assertCount(1, $settings->paths);
        $this->assertEquals('.', $settings->paths[0]);
    }

    public function test_archivesonly_arg()
    {
        $args = ['--archivesonly'];
        $settings = $this->searchoptions->settings_from_args($args);
        $this->assertTrue($settings->archivesonly);
        $this->assertTrue($settings->searcharchives);
    }

    public function test_debug_arg()
    {
        $args = ['--debug'];
        $settings = $this->searchoptions->settings_from_args($args);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
    }

    public function test_missing_arg()
    {
        $this->expectException(SearchException::class);
        $args = ['-x', 'php,py', '-s', 'Search', '.', '-D'];
        $this->searchoptions->settings_from_args($args);
    }

    public function test_invalid_arg()
    {
        $this->expectException(SearchException::class);
        $args = ['-x', 'php,py', '-s', 'Search', '.', '-Q'];
        $this->searchoptions->settings_from_args($args);
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
        $this->searchoptions->settings_from_json($json, $settings);
        $this->assertCount(1, $settings->paths);
        $this->assertTrue(in_array('~/src/xsearch/', $settings->paths));
        $this->assertCount(2, $settings->in_extensions);
        $this->assertTrue(in_array('js', $settings->in_extensions));
        $this->assertTrue(in_array('ts', $settings->in_extensions));
        $this->assertCount(1, $settings->out_dirpatterns);
        $this->assertEquals('node_module', $settings->out_dirpatterns[0]);
        $this->assertCount(1, $settings->out_filepatterns);
        $this->assertEquals('temp', $settings->out_filepatterns[0]);
        $this->assertCount(1, $settings->searchpatterns);
        $this->assertEquals('Searcher', $settings->searchpatterns[0]);
        $this->assertEquals(2, $settings->linesbefore);
        $this->assertEquals(2, $settings->linesafter);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
        $this->assertTrue($settings->firstmatch);
        $this->assertTrue(!$settings->excludehidden);
    }
}
