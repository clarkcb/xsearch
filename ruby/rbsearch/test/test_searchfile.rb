################################################################################
#
# searchfile_test.rb
#
# Test the SearchFile class
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'

module RbSearch

  class SearchFileTest < Minitest::Test
    def test_searchfile_abs_path
      path = '/Users/cary/src/xsearch/ruby/rbsearch'
      filename = 'searchfile.rb'
      searchfile = SearchFile.new(path, filename, FileType::CODE)
      assert_equal('/Users/cary/src/xsearch/ruby/rbsearch/searchfile.rb', searchfile.relativepath)
    end

    def test_searchfile_rel_path1
      path = '.'
      filename = 'searchfile.rb'
      searchfile = SearchFile.new(path, filename, FileType::CODE)
      assert_equal('./searchfile.rb', searchfile.relativepath)
    end

    def test_searchfile_rel_path2
      path = './'
      filename = 'searchfile.rb'
      searchfile = SearchFile.new(path, filename, FileType::CODE)
      assert_equal('./searchfile.rb', searchfile.relativepath)
    end

    def test_searchfile_rel_path3
      path = '..'
      filename = 'searchfile.rb'
      searchfile = SearchFile.new(path, filename, FileType::CODE)
      assert_equal('../searchfile.rb', searchfile.relativepath)
    end
  end
end
