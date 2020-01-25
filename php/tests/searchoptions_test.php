<?php

class SearchOptionsTest extends PHPUnit_Framework_TestCase
{
    public function __construct()
    {
        parent::__construct();
        $this->searchoptions = new SearchOptions();
    }

    public function test_no_args()
    {
        $settings = $this->searchoptions->settings_from_args([]);
        $this->assertFalse($settings->archivesonly);
        $this->assertFalse($settings->debug);
        $this->assertTrue($settings->excludehidden);
        $this->assertFalse($settings->firstmatch);
        $this->assertEquals($settings->linesafter, 0);
        $this->assertEquals($settings->linesbefore, 0);
        $this->assertFalse($settings->listdirs);
        $this->assertFalse($settings->listfiles);
        $this->assertFalse($settings->listlines);
        $this->assertEquals($settings->maxlinelength, 150);
        $this->assertFalse($settings->multilinesearch);
        $this->assertTrue($settings->printresults);
        $this->assertFalse($settings->printusage);
        $this->assertFalse($settings->printversion);
        $this->assertTrue($settings->recursive);
        $this->assertFalse($settings->searcharchives);
        $this->assertFalse(isset($settings->startpath));
        $this->assertFalse($settings->uniquelines);
        $this->assertFalse($settings->verbose);
    }

    public function test_valid_args()
    {
        $args = ['-x', 'php,py', '-s', 'Search', '.'];
        $settings = $this->searchoptions->settings_from_args($args);
        $this->assertEquals(count($settings->in_extensions), 2);
        $this->assertTrue(in_array('php', $settings->in_extensions));
        $this->assertTrue(in_array('py', $settings->in_extensions));
        $this->assertEquals(count($settings->searchpatterns), 1);
        $this->assertEquals($settings->searchpatterns[0], 'Search');
        $this->assertEquals($settings->startpath, '.');
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

    /**
    * @expectedException SearchException
    */
    public function test_missing_arg()
    {
        $args = ['-x', 'php,py', '-s', 'Search', '.', '-D'];
        $settings = $this->searchoptions->settings_from_args($args);
    }

    /**
    * @expectedException SearchException
    */
    public function test_invalid_arg()
    {
        $args = ['-x', 'php,py', '-s', 'Search', '.', '-Q'];
        $settings = $this->searchoptions->settings_from_args($args);
    }

    public function test_settings_from_json()
    {
        $settings = new SearchSettings();
        $json = <<<"END_JSON"
{
  "startpath": "~/src/xsearch/",
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
        $this->assertEquals($settings->startpath, '~/src/xsearch/');
        $this->assertEquals(count($settings->in_extensions), 2);
        $this->assertTrue(in_array('js', $settings->in_extensions));
        $this->assertTrue(in_array('ts', $settings->in_extensions));
        $this->assertEquals(count($settings->out_dirpatterns), 1);
        $this->assertEquals($settings->out_dirpatterns[0], 'node_module');
        $this->assertEquals(count($settings->out_filepatterns), 1);
        $this->assertEquals($settings->out_filepatterns[0], 'temp');
        $this->assertEquals(count($settings->searchpatterns), 1);
        $this->assertEquals($settings->searchpatterns[0], 'Searcher');
        $this->assertEquals($settings->linesbefore, 2);
        $this->assertEquals($settings->linesafter, 2);
        $this->assertTrue($settings->debug);
        $this->assertTrue($settings->verbose);
        $this->assertTrue($settings->firstmatch);
        $this->assertTrue(!$settings->excludehidden);
    }
}
