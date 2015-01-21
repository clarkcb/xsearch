################################################################################
#
# fileutil_test.rb
#
# Test the FileUtil static class
#
################################################################################

require_relative "../rbsearch/fileutil.rb"
require "test/unit"
 
class FileUtilTest < Test::Unit::TestCase
  def test_get_extension_has_txt_extension
    filename = 'filename.txt'
    assert_equal(FileUtil.get_extension(filename), 'txt')
  end

  def test_get_extension_missing_extension
    filename = 'filename.'
    assert_equal(FileUtil.get_extension(filename), '')
  end

  def test_get_extension_no_extension
    filename = 'filename'
    assert_equal(FileUtil.get_extension(filename), '')
  end

  def test_get_extension_hidden_txt_file
    filename = '.hidden.txt'
    assert_equal(FileUtil.get_extension(filename), 'txt')
  end

  def test_get_extension_hidden_file_missing_extension
    filename = '.hidden.'
    assert_equal(FileUtil.get_extension(filename), '')
  end

  def test_get_extension_hidden_file_no_extension
    filename = '.hidden'
    assert_equal(FileUtil.get_extension(filename), '')
  end
end
