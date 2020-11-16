################################################################################
#
# fileutil_test.rb
#
# Test the FileUtil static class
#
################################################################################

require_relative '../lib/rbsearch'
require 'test/unit'

class FileUtilTest < Test::Unit::TestCase
################################################################################
# get_extension tests
################################################################################
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

################################################################################
# is_dot_dir tests
################################################################################
  def test_is_dot_dir_single_dot
    filename = '.'
    assert(FileUtil.dot_dir?(filename))
  end

  def test_is_dot_dir_double_dot
    filename = '..'
    assert(FileUtil.dot_dir?(filename))
  end

  def test_is_dot_dir_non_dot_dir
    filename = '.git'
    assert(!FileUtil.dot_dir?(filename))
  end

################################################################################
# is_hidden tests
################################################################################
  def test_is_hidden_hidden_file
    filename = '.filename.txt'
    assert(FileUtil.hidden?(filename))
  end

  def test_is_hidden_not_hidden_file
    filename = 'filename.txt'
    assert(!FileUtil.hidden?(filename))
  end

  def test_is_hidden_single_dot
    filename = '.'
    assert(!FileUtil.hidden?(filename))
  end

  def test_is_hidden_double_dot
    filename = '..'
    assert(!FileUtil.hidden?(filename))
  end
end
