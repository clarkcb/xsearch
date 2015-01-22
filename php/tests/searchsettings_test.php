<?php

class SearchSettingsTest extends PHPUnit_Framework_TestCase {
    function __construct() {
        parent::__construct();
        $this->settings = new SearchSettings();
    }

    public function test_default_settings() {
        $this->assertFalse($this->settings->archivesonly);
        $this->assertFalse($this->settings->debug);
        $this->assertFalse($this->settings->dotiming);
        $this->assertTrue($this->settings->excludehidden);
        $this->assertFalse($this->settings->firstmatch);
        $this->assertEquals($this->settings->linesafter, 0);
        $this->assertEquals($this->settings->linesbefore, 0);
        $this->assertFalse($this->settings->listdirs);
        $this->assertFalse($this->settings->listfiles);
        $this->assertFalse($this->settings->listlines);
        $this->assertEquals($this->settings->maxlinelength, 150);
        $this->assertFalse($this->settings->multilinesearch);
        $this->assertTrue($this->settings->printresults);
        $this->assertFalse($this->settings->printusage);
        $this->assertFalse($this->settings->printversion);
        $this->assertTrue($this->settings->recursive);
        $this->assertFalse($this->settings->searcharchives);
        $this->assertFalse(isset($this->settings->startpath));
        $this->assertFalse($this->settings->uniquelines);
        $this->assertFalse($this->settings->verbose);
    }

    public function test_set_properties() {
        $this->settings->archivesonly = true;
        $this->settings->debug = true;
        $this->settings->dotiming = true;
        $this->settings->linesafter = 5;
        $this->settings->linesbefore = 5;
        $this->assertTrue($this->settings->archivesonly);
        $this->assertTrue($this->settings->debug);
        $this->assertTrue($this->settings->dotiming);
        $this->assertEquals($this->settings->linesafter, 5);
        $this->assertEquals($this->settings->linesbefore, 5);
    }

    public function test_add_single_extension() {
        $this->settings->add_exts('php', $this->settings->in_extensions);
        $this->assertEquals(count($this->settings->in_extensions), 1);
        $this->assertEquals($this->settings->in_extensions[0], 'php');
    }

    public function test_add_comma_delimited_extensions() {
        $this->settings->add_exts('php,py', $this->settings->in_extensions);
        $this->assertEquals(count($this->settings->in_extensions), 2);
        $this->assertTrue(in_array('php', $this->settings->in_extensions));
        $this->assertTrue(in_array('py', $this->settings->in_extensions));
    }
}

?>