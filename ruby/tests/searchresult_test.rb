################################################################################
#
# searchresult_test.rb
#
# Test the SearchResult class
#
################################################################################

require_relative "../rbsearch/config.rb"
require_relative "../rbsearch/searchresult.rb"
require "test/unit"
 
class SearchResultTest < Test::Unit::TestCase
  @cssearch_path = "#{XSEARCHPATH}/csharp/CsSearch/CsSearch"
  def test_singleline_searchresult
    pattern = "Search"
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
    assert_equal(searchresult.to_s, expectedoutput)
  end

  def test_binaryfile_searchresult
    pattern = "Search"
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
    assert_equal(searchresult.to_s, expectedoutput)
  end

  def test_multiline_searchresult
    pattern = "Search"
    filepath = "#{@cssearch_path}/Searcher.cs"
    linenum = 10
    match_start_index = 15
    match_end_index = 23
    line = "\tpublic class Searcher\n"
    linesbefore = ["namespace CsSearch\n", "{\n"]
    linesafter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"]
    searchresult = SearchResult.new(pattern, filepath, linenum, match_start_index,
      match_end_index, line, linesbefore, linesafter)
    outputtemplate = <<-eos
================================================================================
%s: %d: [%d:%d]
--------------------------------------------------------------------------------
   8 | namespace CsSearch
   9 | {
> 10 | \tpublic class Searcher
  11 | \t{
  12 | \t\tprivate readonly FileTypes _fileTypes;

eos
    expectedoutput = outputtemplate % [filepath, linenum, match_start_index, match_end_index]
    assert_equal(searchresult.to_s, expectedoutput)
  end
end
