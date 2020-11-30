################################################################################
#
# searchresult_test.rb
#
# Test the SearchResult class
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'
 
class SearchResultTest < Minitest::Test
  def setup
    @cssearch_path = "~/src/xsearch/csharp/CsSearch/CsSearch"
  end

  def test_singleline_searchresult
    settings = SearchSettings::new()
    settings.colorize = false
    formatter = SearchResultFormatter::new(settings)
    pattern = 'Search'
    filepath = "#{@cssearch_path}/Searcher.cs"
    linenum = 10
    match_start_index = 15
    match_end_index = 23
    line = "\tpublic class Searcher\n"
    linesbefore = []
    linesafter = []
    searchresult = SearchResult.new(pattern, filepath, linenum, match_start_index,
      match_end_index, line, linesbefore, linesafter)
    expectedoutput = "#{filepath}: #{linenum}: [#{match_start_index}:#{match_end_index}]:"
    expectedoutput += " #{line.strip}"
    output = formatter.format(searchresult)
    assert_equal(expectedoutput, output)
  end

  def test_singleline_longer_than_maxlength_searchresult
    settings = SearchSettings::new()
    settings.colorize = false
    settings.maxlinelength = 100
    formatter = SearchResultFormatter::new(settings)
    pattern = 'pattern'
    filepath = "./maxlen.txt"
    linenum = 1
    match_start_index = 53
    match_end_index = 59
    line = '0123456789012345678901234567890123456789012345678901' \
           'maxlen' \
           '8901234567890123456789012345678901234567890123456789'
    linesbefore = []
    linesafter = []
    searchresult = SearchResult.new(pattern, filepath, linenum, match_start_index,
      match_end_index, line, linesbefore, linesafter)
    expectedline = '...89012345678901234567890123456789012345678901' \
                   'maxlen' \
                   '89012345678901234567890123456789012345678901...'
    expectedoutput = "#{filepath}: #{linenum}: [#{match_start_index}:#{match_end_index}]:"
    expectedoutput += " #{expectedline}"
    output = formatter.format(searchresult)
    assert_equal(expectedoutput, output)
  end

  def test_singleline_longer_colorize_searchresult
    settings = SearchSettings::new()
    settings.colorize = true
    settings.maxlinelength = 100
    formatter = SearchResultFormatter::new(settings)
    pattern = 'pattern'
    filepath = "./maxlen.txt"
    linenum = 1
    match_start_index = 53
    match_end_index = 59
    line = '0123456789012345678901234567890123456789012345678901' \
           'maxlen' \
           '8901234567890123456789012345678901234567890123456789'
    linesbefore = []
    linesafter = []
    searchresult = SearchResult.new(pattern, filepath, linenum, match_start_index,
      match_end_index, line, linesbefore, linesafter)
    expectedline = '...89012345678901234567890123456789012345678901' +
                   'maxlen'.green +
                   '89012345678901234567890123456789012345678901...'
    expectedoutput = "#{filepath}: #{linenum}: [#{match_start_index}:#{match_end_index}]:"
    expectedoutput += " #{expectedline}"
    output = formatter.format(searchresult)
    assert_equal(expectedoutput, output)
  end

  def test_binaryfile_searchresult
    settings = SearchSettings::new()
    formatter = SearchResultFormatter::new(settings)
    pattern = 'Search'
    filepath = "#{@cssearch_path}/Searcher.exe"
    linenum = 0
    match_start_index = 0
    match_end_index = 0
    line = nil
    linesbefore = []
    linesafter = []
    searchresult = SearchResult.new(pattern, filepath, linenum, match_start_index,
      match_end_index, line, linesbefore, linesafter)
    expectedoutput = "#{filepath} matches at [0:0]"
    output = formatter.format(searchresult)
    assert_equal(expectedoutput, output)
  end

  def test_multiline_searchresult
    settings = SearchSettings::new()
    settings.colorize = false
    formatter = SearchResultFormatter::new(settings)
    pattern = 'Search'
    filepath = "#{@cssearch_path}/Searcher.cs"
    linenum = 10
    match_start_index = 15
    match_end_index = 23
    line = "\tpublic class Searcher\n"
    linesbefore = ["namespace CsSearch\n", "{\n"]
    linesafter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"]
    searchresult = SearchResult.new(pattern, filepath, linenum, match_start_index,
      match_end_index, line, linesbefore, linesafter)
    outputtemplate = <<~OUTPUT_TEMPLATE
      ================================================================================
      %s: %d: [%d:%d]
      --------------------------------------------------------------------------------
         8 | namespace CsSearch
         9 | {
      > 10 | \tpublic class Searcher
        11 | \t{
        12 | \t\tprivate readonly FileTypes _fileTypes;

    OUTPUT_TEMPLATE
    expectedoutput = outputtemplate % [filepath, linenum, match_start_index, match_end_index]
    output = formatter.format(searchresult)
    assert_equal(expectedoutput, output)
  end
end
