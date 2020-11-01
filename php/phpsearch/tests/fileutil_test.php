<?php

class FileUtilTest extends PHPUnit_Framework_TestCase
{

    /***************************************************************************
     * get_extension tests
     **************************************************************************/
    public function test_get_extension_has_txt_extension()
    {
        $filename = 'filename.txt';
        $this->assertEquals(FileUtil::get_extension($filename), "txt");
    }

    public function test_get_extension_missing_extension()
    {
        $filename = 'filename.';
        $this->assertEquals(FileUtil::get_extension($filename), "");
    }

    public function test_get_extension_no_extension()
    {
        $filename = 'filename';
        $this->assertEquals(FileUtil::get_extension($filename), "");
    }

    public function test_get_extension_hidden_txt_extension()
    {
        $filename = '.filename.txt';
        $this->assertEquals(FileUtil::get_extension($filename), "txt");
    }

    public function test_get_extension_hidden_missing_extension()
    {
        $filename = '.filename.';
        $this->assertEquals(FileUtil::get_extension($filename), "");
    }

    public function test_get_extension_hidden_no_extension()
    {
        $filename = '.filename';
        $this->assertEquals(FileUtil::get_extension($filename), "");
    }

    /***************************************************************************
     * is_dot_dir tests
     **************************************************************************/
    public function test_is_dot_dir_single_dot()
    {
        $filename = '.';
        $this->assertTrue(FileUtil::is_dot_dir($filename));
    }

    public function test_is_dot_dir_double_dot()
    {
        $filename = '..';
        $this->assertTrue(FileUtil::is_dot_dir($filename));
    }

    public function test_is_dot_dir_non_dot_dir()
    {
        $filename = '.git';
        $this->assertFalse(FileUtil::is_dot_dir($filename));
    }

    /***************************************************************************
     * is_hidden tests
     **************************************************************************/
    public function test_is_hidden_hidden_file()
    {
        $filename = '.filename.txt';
        $this->assertTrue(FileUtil::is_hidden($filename));
    }

    public function test_is_hidden_not_hidden_file()
    {
        $filename = 'filename.txt';
        $this->assertFalse(FileUtil::is_hidden($filename));
    }

    public function test_is_hidden_single_dot()
    {
        $filename = '.';
        $this->assertFalse(FileUtil::is_hidden($filename));
    }

    public function test_is_hidden_double_dot()
    {
        $filename = '..';
        $this->assertFalse(FileUtil::is_hidden($filename));
    }

    /***************************************************************************
     * join_path tests
     **************************************************************************/
    public function test_join_path_forward_slashes()
    {
        $path = '/path/to/nowhere';
        $file = 'nowhere.txt';
        $joined = FileUtil::join_path($path, $file);
        $this->assertEquals($joined, '/path/to/nowhere/nowhere.txt');
    }

    public function test_join_path_backslashes()
    {
        $path = 'C:\\path\\to\\nowhere';
        $file = 'nowhere.txt';
        $joined = FileUtil::join_path($path, $file);
        $this->assertEquals($joined, 'C:\\path\\to\\nowhere\\nowhere.txt');
    }

    public function test_join_path_no_slashes()
    {
        $path = 'nowhere';
        $file = 'nowhere.txt';
        $joined = FileUtil::join_path($path, $file);
        $this->assertEquals($joined, 'nowhere/nowhere.txt');
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
            FileUtil::normalize_path($path),
            '/path/to/nowhere'
        );
    }

    public function test_normalize_path_trailing_backslash()
    {
        $path = 'C:\\path\\to\\nowhere\\';
        $this->assertEquals(
            FileUtil::normalize_path($path),
            'C:\\path\\to\\nowhere'
        );
    }

    /***************************************************************************
     * split_path tests
     **************************************************************************/
    public function test_split_path_no_sep()
    {
        $path = 'nowhere';
        $split = FileUtil::split_path($path);
        $this->assertEquals(count($split), 1);
        $this->assertEquals($split[0], 'nowhere');
    }

    public function test_split_path_slashes()
    {
        $path = '/path/to/nowhere';
        $split = FileUtil::split_path($path);
        $this->assertEquals(count($split), 4);
        $this->assertEquals($split[0], '');
        $this->assertEquals($split[1], 'path');
        $this->assertEquals($split[2], 'to');
        $this->assertEquals($split[3], 'nowhere');
    }
}
