################################################################################
#
# searcher_test.rb
#
# Searcher testing
#
################################################################################

require_relative '../lib/rbsearch'
require 'test/unit'

module RbSearch

  class SearcherTest < Test::Unit::TestCase

    def get_settings
      settings = SearchSettings.new
      settings.paths.add(Pathname.new('.'))
      settings.add_pattern('Searcher', settings.search_patterns)
      settings
    end

    def get_test_file
      # File.expand_path("#{SHAREDPATH}/testFiles/testFile2.txt")
      Pathname.new(File.dirname(__FILE__)).join("fixtures/testFile2.txt")
    end

    ################################################################################
    # search_lines tests
    ################################################################################
    def test_search_lines
      settings = get_settings
      searcher = Searcher.new(settings)
      testfile = get_test_file
      fo = File.open(testfile, mode: 'r:ISO-8859-1')
      contents = fo.read
      results = searcher.search_multi_line_string(contents)
      assert_equal(2, results.size)

      first_result = results[0]
      assert_equal(30, first_result.line_num)
      assert_equal(3, first_result.match_start_index)
      assert_equal(11, first_result.match_end_index)

      # swap arguments to assert_equal to match the order of the original code
      second_result = results[1]
      assert_equal(36, second_result.line_num)
      assert_equal(24, second_result.match_start_index)
      assert_equal(32, second_result.match_end_index)
    ensure
      fo.close
    end

    ################################################################################
    # search_multi_line_string tests
    ################################################################################
    def test_search_multi_line_string
      settings = get_settings
      searcher = Searcher.new(settings)
      test_file = get_test_file
      fo = File.open(test_file, mode: 'r:ISO-8859-1')
      line_iterator = fo.each_line
      results = searcher.search_line_iterator(line_iterator)
      assert_equal(results.size, 2)

      first_result = results[0]
      assert_equal(30, first_result.line_num)
      assert_equal(3, first_result.match_start_index)
      assert_equal(11, first_result.match_end_index)

      second_result = results[1]
      assert_equal(36, second_result.line_num)
      assert_equal(24, second_result.match_start_index)
      assert_equal(32, second_result.match_end_index)
    ensure
      fo.close
    end
  end
end
