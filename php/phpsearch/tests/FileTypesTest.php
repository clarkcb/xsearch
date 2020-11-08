<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

class FileTypesTest extends TestCase
{
    /**
     * @var FileTypes
     */
    private $filetypes;

    public function __construct()
    {
        parent::__construct();
        $this->filetypes = new FileTypes();
    }

    public function test_getfiletype_archive_file()
    {
        $filename = 'archive.zip';
        $this->assertEquals(true, $this->filetypes->is_archive($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Archive, $filetype);
    }

    public function test_getfiletype_binary_file()
    {
        $filename = 'binary.exe';
        $this->assertEquals(true, $this->filetypes->is_binary($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Binary, $filetype);
    }

    public function test_getfiletype_code_file()
    {
        $filename = 'code.php';
        $this->assertEquals(true, $this->filetypes->is_code($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Code, $filetype);
    }

    public function test_getfiletype_text_file()
    {
        $filename = 'text.txt';
        $this->assertEquals(true, $this->filetypes->is_text($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Text, $filetype);
    }

    public function test_getfiletype_xml_file()
    {
        $filename = 'content.xml';
        $this->assertEquals(true, $this->filetypes->is_xml($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Xml, $filetype);
    }

    public function test_getfiletype_searchable_file()
    {
        $filename = 'compressed.bz2';
        $this->assertEquals(true, $this->filetypes->is_searchable($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Archive, $filetype);
    }

    public function test_getfiletype_unknown_file()
    {
        $filename = 'unknown.xyz';
        $this->assertEquals(true, $this->filetypes->is_unknown($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Unknown, $filetype);
    }
}
