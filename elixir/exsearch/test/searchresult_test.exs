defmodule ExSearchTest.SearchResultTest do
  alias ExFind.FileResult
  alias ExSearch.SearchResult
  alias ExSearch.SearchResultFormatter
  alias ExSearch.SearchSettings
  use ExUnit.Case
  doctest ExSearch.SearchResult

  test "search_result is valid" do
    pattern = ~r/Seacher/
    path = "."
    file_name = "exsearch.ex"
    file_result = FileResult.new(path, file_name, :code, 0, 0)
    line_num = 7
    line = "  alias ExSearch.Searcher"
    lines_before = []
    lines_after = []
    match_start_index = 18
    match_end_index = 26
    search_result = SearchResult.new(pattern, file_result, line_num, line, lines_before, lines_after, match_start_index, match_end_index)
    assert search_result.file.path == path
    assert search_result.file.name == file_name
    assert search_result.file.file_type == :code
    assert search_result.file.file_size == 0
    assert search_result.file.last_mod == 0
    assert search_result.line_num == line_num
    assert search_result.line == line
    assert search_result.lines_before == lines_before
    assert search_result.lines_after == lines_after
    assert search_result.match_start_index == match_start_index
    assert search_result.match_end_index == match_end_index
    # assert FileResult.to_string(search_result) == "./exsearch.ex: 7: [18:26]: alias ExSearch.Searcher"
    assert FileResult.to_string(search_result.file) == "./exsearch.ex"
  end

  test "single-line search_result is formatted correctly" do
    settings = SearchSettings.new(colorize: false)
    pattern = ~r/Seacher/
    path = "elixir/exsearch/lib"
    file_name = "exsearch.ex"
    file_result = FileResult.new(path, file_name, :code, 0, 0)
    line_num = 7
    line = "  alias ExSearch.Searcher"
    lines_before = []
    lines_after = []
    match_start_index = 18
    match_end_index = 26
    search_result = SearchResult.new(pattern, file_result, line_num, line, lines_before, lines_after, match_start_index, match_end_index)
    search_result_formatted = SearchResultFormatter.format(search_result, settings)
    assert search_result_formatted == "elixir/exsearch/lib/exsearch.ex: 7: [18:26]: alias ExSearch.Searcher"
  end

  test "multi-line search_result is formatted correctly" do
    settings = SearchSettings.new(colorize: false, lines_before: 2, lines_after: 2)
    pattern = ~r/Seacher/
    path = "elixir/exsearch/lib"
    file_name = "exsearch.ex"
    file_result = FileResult.new(path, file_name, :code, 0, 0)
    line_num = 7
    line = "  alias ExSearch.Searcher"
    lines_before = ["", "  alias ExFind.FileResult"]
    lines_after = ["  alias ExSearch.SearchOptions", "  alias ExSearch.SearchResultFormatter"]
    match_start_index = 18
    match_end_index = 26
    search_result = SearchResult.new(pattern, file_result, line_num, line, lines_before, lines_after, match_start_index, match_end_index)
    search_result_formatted = SearchResultFormatter.format(search_result, settings)
    expected_result_formatted = [
      "================================================================================",
      "elixir/exsearch/lib/exsearch.ex: 7: [18:26]",
      "--------------------------------------------------------------------------------",
      "  5 | ",
      "  6 |   alias ExFind.FileResult",
      "> 7 |   alias ExSearch.Searcher",
      "  8 |   alias ExSearch.SearchOptions",
      "  9 |   alias ExSearch.SearchResultFormatter\n"
    ]
    assert search_result_formatted == Enum.join(expected_result_formatted, "\n")
  end
end
