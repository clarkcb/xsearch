################################################################################
#
# search_result_test.rb
#
# Test the SearchResult class
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'

module RbSearch

  class SearchResultTest < Test::Unit::TestCase
    def setup
      @cssearch_path = "~/src/xsearch/csharp/CsSearch/CsSearch"
    end

    def test_singleline_search_result
      settings = SearchSettings::new()
      settings.colorize = false
      formatter = SearchResultFormatter::new(settings)
      pattern = 'Search'
      file_path = "#{@cssearch_path}/Searcher.cs"
      line_num = 10
      match_start_index = 15
      match_end_index = 23
      line = "\tpublic class Searcher\n"
      lines_before = []
      lines_after = []
      search_result = SearchResult.new(pattern, file_path, line_num, match_start_index,
                                      match_end_index, line, lines_before, lines_after)
      expected_output = "#{file_path}: #{line_num}: [#{match_start_index}:#{match_end_index}]:"
      expected_output += " #{line.strip}"
      output = formatter.format(search_result)
      assert_equal(expected_output, output)
    end

    def test_singleline_longer_than_maxlength_search_result
      settings = SearchSettings::new()
      settings.colorize = false
      settings.max_line_length = 100
      formatter = SearchResultFormatter::new(settings)
      pattern = 'pattern'
      file_path = "./maxlen.txt"
      line_num = 1
      match_start_index = 53
      match_end_index = 59
      line = '0123456789012345678901234567890123456789012345678901' \
           'maxlen' \
           '8901234567890123456789012345678901234567890123456789'
      lines_before = []
      lines_after = []
      search_result = SearchResult.new(pattern, file_path, line_num, match_start_index,
                                      match_end_index, line, lines_before, lines_after)
      expected_line = '...89012345678901234567890123456789012345678901' \
                   'maxlen' \
                   '89012345678901234567890123456789012345678901...'
      expected_output = "#{file_path}: #{line_num}: [#{match_start_index}:#{match_end_index}]:"
      expected_output += " #{expected_line}"
      output = formatter.format(search_result)
      assert_equal(expected_output, output)
    end

    def test_single_line_longer_colorize_search_result
      settings = SearchSettings::new()
      settings.colorize = true
      settings.max_line_length = 100
      formatter = SearchResultFormatter::new(settings)
      pattern = 'pattern'
      file_path = "./maxlen.txt"
      line_num = 1
      match_start_index = 53
      match_end_index = 59
      line = '0123456789012345678901234567890123456789012345678901' \
           'maxlen' \
           '8901234567890123456789012345678901234567890123456789'
      lines_before = []
      lines_after = []
      search_result = SearchResult.new(pattern, file_path, line_num, match_start_index,
                                      match_end_index, line, lines_before, lines_after)
      expected_line = '...89012345678901234567890123456789012345678901' +
        'maxlen'.green +
        '89012345678901234567890123456789012345678901...'
      expected_output = "#{file_path}: #{line_num}: [#{match_start_index}:#{match_end_index}]:"
      expected_output += " #{expected_line}"
      output = formatter.format(search_result)
      assert_equal(expected_output, output)
    end

    def test_binary_file_search_result
      settings = SearchSettings::new()
      formatter = SearchResultFormatter::new(settings)
      pattern = 'Search'
      file_path = "#{@cssearch_path}/Searcher.exe"
      line_num = 0
      match_start_index = 0
      match_end_index = 0
      line = nil
      lines_before = []
      lines_after = []
      search_result = SearchResult.new(pattern, file_path, line_num, match_start_index,
                                      match_end_index, line, lines_before, lines_after)
      expected_output = "#{file_path} matches at [0:0]"
      output = formatter.format(search_result)
      assert_equal(expected_output, output)
    end

    def test_multiline_search_result
      settings = SearchSettings::new()
      settings.colorize = false
      formatter = SearchResultFormatter::new(settings)
      pattern = 'Search'
      file_path = "#{@cssearch_path}/Searcher.cs"
      line_num = 10
      match_start_index = 15
      match_end_index = 23
      line = "\tpublic class Searcher\n"
      lines_before = ["namespace CsSearch\n", "{\n"]
      lines_after = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"]
      search_result = SearchResult.new(pattern, file_path, line_num, match_start_index,
                                      match_end_index, line, lines_before, lines_after)
      output_template = <<~OUTPUT_TEMPLATE
      ================================================================================
      %s: %d: [%d:%d]
      --------------------------------------------------------------------------------
         8 | namespace CsSearch
         9 | {
      > 10 | \tpublic class Searcher
        11 | \t{
        12 | \t\tprivate readonly FileTypes _fileTypes;

      OUTPUT_TEMPLATE
      expected_output = output_template % [file_path, line_num, match_start_index, match_end_index]
      output = formatter.format(search_result)
      assert_equal(expected_output, output)
    end
  end
end
