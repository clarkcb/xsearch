<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpsearch\FileType;
use phpsearch\SearchFile;

class SearchFileTest extends TestCase
{
    public function test_search_file_abs_path()
    {
        $path = '/Users/cary/src/xsearch/php/phpsearch/src';
        $file_name = 'searchfile.php';
        $search_file = new SearchFile($path, $file_name, FileType::Code);
        $this->assertEquals('/Users/cary/src/xsearch/php/phpsearch/src/search_file.php',
            $search_file->file_path());
    }

    public function test_search_file_rel_path1()
    {
        $path = '.';
        $file_name = 'searchfile.php';
        $search_file = new SearchFile($path, $file_name, FileType::Code);
        $this->assertEquals('./search_file.php', $search_file->file_path());
    }

    public function test_search_file_rel_path2()
    {
        $path = './';
        $file_name = 'searchfile.php';
        $search_file = new SearchFile($path, $file_name, FileType::Code);
        $this->assertEquals('./search_file.php', $search_file->file_path());
    }

    public function test_search_file_rel_path3()
    {
        $path = '..';
        $file_name = 'searchfile.php';
        $search_file = new SearchFile($path, $file_name, FileType::Code);
        $this->assertEquals('../search_file.php', $search_file->file_path());
    }
}
