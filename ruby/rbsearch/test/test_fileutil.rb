################################################################################
#
# fileutil_test.rb
#
# Test the FileUtil static class
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'

module RbSearch

  class FileUtilTest < Minitest::Test
    ################################################################################
    # get_extension tests
    ################################################################################
    def test_get_extension_has_txt_extension
      file_name = 'filename.txt'
      assert_equal(FileUtil.get_extension(file_name), 'txt')
    end

    def test_get_extension_missing_extension
      file_name = 'filename.'
      assert_equal(FileUtil.get_extension(file_name), '')
    end

    def test_get_extension_no_extension
      file_name = 'filename'
      assert_equal(FileUtil.get_extension(file_name), '')
    end

    def test_get_extension_hidden_txt_file
      file_name = '.hidden.txt'
      assert_equal(FileUtil.get_extension(file_name), 'txt')
    end

    def test_get_extension_hidden_file_missing_extension
      file_name = '.hidden.'
      assert_equal(FileUtil.get_extension(file_name), '')
    end

    def test_get_extension_hidden_file_no_extension
      file_name = '.hidden'
      assert_equal(FileUtil.get_extension(file_name), '')
    end

    ################################################################################
    # is_dot_dir tests
    ################################################################################
    def test_is_dot_dir_single_dot
      file_name = '.'
      assert(FileUtil.dot_dir?(file_name))
    end

    def test_is_dot_dir_double_dot
      file_name = '..'
      assert(FileUtil.dot_dir?(file_name))
    end

    def test_is_dot_dir_non_dot_dir
      file_name = '.git'
      assert(!FileUtil.dot_dir?(file_name))
    end

    ################################################################################
    # is_hidden tests
    ################################################################################
    def test_is_hidden_hidden_file
      file_name = '.filename.txt'
      assert(FileUtil.hidden?(file_name))
    end

    def test_is_hidden_not_hidden_file
      file_name = 'filename.txt'
      assert(!FileUtil.hidden?(file_name))
    end

    def test_is_hidden_single_dot
      file_name = '.'
      assert(!FileUtil.hidden?(file_name))
    end

    def test_is_hidden_double_dot
      file_name = '..'
      assert(!FileUtil.hidden?(file_name))
    end
  end
end
