<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpsearch\FileType;
use phpsearch\FileTypes;

class FileTypesTest extends TestCase
{
    /**
     * @var FileTypes
     */
    private $file_types;

    public function __construct()
    {
        parent::__construct();
        $this->file_types = new FileTypes();
    }

    public function test_get_file_type_archive_file()
    {
        $file_name = 'archive.zip';
        $this->assertEquals(true, $this->file_types->is_archive($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Archive, $file_type);
    }

    public function test_get_file_type_binary_file()
    {
        $file_name = 'binary.exe';
        $this->assertEquals(true, $this->file_types->is_binary($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Binary, $file_type);
    }

    public function test_get_file_type_code_file()
    {
        $file_name = 'code.php';
        $this->assertEquals(true, $this->file_types->is_code($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Code, $file_type);
    }

    public function test_get_file_type_text_file()
    {
        $file_name = 'text.txt';
        $this->assertEquals(true, $this->file_types->is_text($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Text, $file_type);
    }

    public function test_get_file_type_xml_file()
    {
        $file_name = 'content.xml';
        $this->assertEquals(true, $this->file_types->is_xml($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Xml, $file_type);
    }

    public function test_get_file_type_searchable_file()
    {
        $file_name = 'compressed.bz2';
        $this->assertEquals(true, $this->file_types->is_searchable($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Archive, $file_type);
    }

    public function test_get_file_type_unknown_file()
    {
        $file_name = 'unknown.xyz';
        $this->assertEquals(true, $this->file_types->is_unknown($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Unknown, $file_type);
    }
}
