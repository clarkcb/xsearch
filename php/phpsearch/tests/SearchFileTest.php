<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

class SearchFileTest extends TestCase
{
    public function test_searchfile_abs_path()
    {
        $path = '/Users/cary/src/xsearch/php/phpsearch/src';
        $filename = 'searchfile.php';
        $searchfile = new SearchFile($path, $filename, FileType::Code);
        $this->assertEquals('/Users/cary/src/xsearch/php/phpsearch/src/searchfile.php',
            $searchfile->filepath());
    }

    public function test_searchfile_rel_path1()
    {
        $path = '.';
        $filename = 'searchfile.php';
        $searchfile = new SearchFile($path, $filename, FileType::Code);
        $this->assertEquals('./searchfile.php', $searchfile->filepath());
    }

    public function test_searchfile_rel_path2()
    {
        $path = './';
        $filename = 'searchfile.php';
        $searchfile = new SearchFile($path, $filename, FileType::Code);
        $this->assertEquals('./searchfile.php', $searchfile->filepath());
    }

    public function test_searchfile_rel_path3()
    {
        $path = '..';
        $filename = 'searchfile.php';
        $searchfile = new SearchFile($path, $filename, FileType::Code);
        $this->assertEquals('../searchfile.php', $searchfile->filepath());
    }
}
