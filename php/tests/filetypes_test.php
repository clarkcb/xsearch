<?php

class FileTypesTest extends PHPUnit_Framework_TestCase
{
    public function __construct()
    {
        parent::__construct();
        $this->filetypes = new FileTypes();
    }

    public function test_getfiletype_archive_file()
    {
        $filename = 'archive.zip';
        $this->assertEquals($this->filetypes->is_archive($filename), true);
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals($filetype, FileType::Archive);
    }

    public function test_getfiletype_binary_file()
    {
        $filename = 'binary.exe';
        $this->assertEquals($this->filetypes->is_binary($filename), true);
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals($filetype, FileType::Binary);
    }

    public function test_getfiletype_text_file()
    {
        $filename = 'text.txt';
        $this->assertEquals($this->filetypes->is_text($filename), true);
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals($filetype, FileType::Text);
    }

    public function test_getfiletype_unknown_file()
    {
        $filename = 'unknown.xyz';
        $this->assertEquals($this->filetypes->is_unknown($filename), true);
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals($filetype, FileType::Unknown);
    }
}
