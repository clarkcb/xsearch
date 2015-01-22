<?php

class FileUtilTest extends PHPUnit_Framework_TestCase {
    public function test_get_extension_has_txt_extension() {
        $filename = 'filename.txt';
        $this->assertEquals(FileUtil::get_extension($filename), "txt");
    }

    public function test_get_extension_missing_extension() {
        $filename = 'filename.';
        $this->assertEquals(FileUtil::get_extension($filename), "");
    }

    public function test_get_extension_no_extension() {
        $filename = 'filename';
        $this->assertEquals(FileUtil::get_extension($filename), "");
    }

    public function test_get_extension_hidden_txt_extension() {
        $filename = '.filename.txt';
        $this->assertEquals(FileUtil::get_extension($filename), "txt");
    }

    public function test_get_extension_hidden_missing_extension() {
        $filename = '.filename.';
        $this->assertEquals(FileUtil::get_extension($filename), "");
    }

    public function test_get_extension_hidden_no_extension() {
        $filename = '.filename';
        $this->assertEquals(FileUtil::get_extension($filename), "");
    }
}

?>