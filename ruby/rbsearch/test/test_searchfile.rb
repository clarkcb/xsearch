################################################################################
#
# search_file_test.rb
#
# Test the SearchFile class
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'

module RbSearch

  class SearchFileTest < Minitest::Test
    def test_search_file_abs_path
      path = '/Users/cary/src/xsearch/ruby/rbsearch'
      file_name = 'searchfile.rb'
      search_file = SearchFile.new(path, file_name, FileType::CODE)
      assert_equal('/Users/cary/src/xsearch/ruby/rbsearch/searchfile.rb', search_file.relative_path)
    end

    def test_search_file_rel_path1
      path = '.'
      file_name = 'searchfile.rb'
      search_file = SearchFile.new(path, file_name, FileType::CODE)
      assert_equal('./searchfile.rb', search_file.relative_path)
    end

    def test_search_file_rel_path2
      path = './'
      file_name = 'searchfile.rb'
      search_file = SearchFile.new(path, file_name, FileType::CODE)
      assert_equal('./searchfile.rb', search_file.relative_path)
    end

    def test_search_file_rel_path3
      path = '..'
      file_name = 'searchfile.rb'
      search_file = SearchFile.new(path, file_name, FileType::CODE)
      assert_equal('../searchfile.rb', search_file.relative_path)
    end
  end
end
