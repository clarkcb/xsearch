<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpsearch\FileUtil;

class FileUtilTest extends TestCase
{
    /***************************************************************************
     * get_extension tests
     **************************************************************************/
    public function test_get_extension_has_txt_extension()
    {
        $file_name = 'filename.txt';
        $this->assertEquals("txt", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_missing_extension()
    {
        $file_name = 'filename.';
        $this->assertEquals("", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_no_extension()
    {
        $file_name = 'filename';
        $this->assertEquals("", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_hidden_txt_extension()
    {
        $file_name = '.filename.txt';
        $this->assertEquals("txt", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_hidden_missing_extension()
    {
        $file_name = '.filename.';
        $this->assertEquals("", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_hidden_no_extension()
    {
        $file_name = '.filename';
        $this->assertEquals("", FileUtil::get_extension($file_name));
    }

    /***************************************************************************
     * is_dot_dir tests
     **************************************************************************/
    public function test_is_dot_dir_single_dot()
    {
        $file_name = '.';
        $this->assertTrue(FileUtil::is_dot_dir($file_name));
    }

    public function test_is_dot_dir_double_dot()
    {
        $file_name = '..';
        $this->assertTrue(FileUtil::is_dot_dir($file_name));
    }

    public function test_is_dot_dir_non_dot_dir()
    {
        $file_name = '.git';
        $this->assertFalse(FileUtil::is_dot_dir($file_name));
    }

    /***************************************************************************
     * is_hidden tests
     **************************************************************************/
    public function test_is_hidden_hidden_file()
    {
        $file_name = '.filename.txt';
        $this->assertTrue(FileUtil::is_hidden($file_name));
    }

    public function test_is_hidden_not_hidden_file()
    {
        $file_name = 'filename.txt';
        $this->assertFalse(FileUtil::is_hidden($file_name));
    }

    public function test_is_hidden_single_dot()
    {
        $file_name = '.';
        $this->assertFalse(FileUtil::is_hidden($file_name));
    }

    public function test_is_hidden_double_dot()
    {
        $file_name = '..';
        $this->assertFalse(FileUtil::is_hidden($file_name));
    }

    /***************************************************************************
     * join_path tests
     **************************************************************************/
    public function test_join_path_forward_slashes()
    {
        $path = '/path/to/nowhere';
        $file = 'nowhere.txt';
        $joined = FileUtil::join_path($path, $file);
        $this->assertEquals('/path/to/nowhere/nowhere.txt', $joined);
    }

    public function test_join_path_backslashes()
    {
        $path = 'C:\\path\\to\\nowhere';
        $file = 'nowhere.txt';
        $joined = FileUtil::join_path($path, $file);
        $this->assertEquals('C:\\path\\to\\nowhere\\nowhere.txt', $joined);
    }

    public function test_join_path_no_slashes()
    {
        $path = 'nowhere';
        $file = 'nowhere.txt';
        $joined = FileUtil::join_path($path, $file);
        $this->assertEquals('nowhere/nowhere.txt', $joined);
    }

    /***************************************************************************
     * normalize_path tests
     **************************************************************************/
    public function test_normalize_path_no_trailing_slash()
    {
        $path = '/path/to/nowhere';
        $this->assertEquals(FileUtil::normalize_path($path), $path);
    }

    public function test_normalize_path_trailing_slash()
    {
        $path = '/path/to/nowhere/';
        $this->assertEquals(
            '/path/to/nowhere',
            FileUtil::normalize_path($path)
        );
    }

    public function test_normalize_path_trailing_backslash()
    {
        $path = 'C:\\path\\to\\nowhere\\';
        $this->assertEquals(
            'C:\\path\\to\\nowhere',
            FileUtil::normalize_path($path)
        );
    }

    /***************************************************************************
     * split_path tests
     **************************************************************************/
    public function test_split_path_no_sep()
    {
        $path = 'nowhere';
        $split = FileUtil::split_path($path);
        $this->assertCount(1, $split);
        $this->assertEquals('nowhere', $split[0]);
    }

    public function test_split_path_slashes()
    {
        $path = '/path/to/nowhere';
        $split = FileUtil::split_path($path);
        $this->assertCount(4, $split);
        $this->assertEquals('', $split[0]);
        $this->assertEquals('path', $split[1]);
        $this->assertEquals('to', $split[2]);
        $this->assertEquals('nowhere', $split[3]);
    }
}
