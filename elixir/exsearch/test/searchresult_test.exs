defmodule ExSearchTest.SearchResultTest do
  alias ExFind.ConsoleColor
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

    settings = SearchSettings.new(colorize: false)
    formatter = SearchResultFormatter.new(settings)

    search_result_formatted = SearchResultFormatter.format(formatter, search_result)
    assert search_result_formatted == "elixir/exsearch/lib/exsearch.ex: 7: [18:26]: alias ExSearch.Searcher"
  end

  test "single-line search_result longer than max_line_length" do
    pattern = ~r/maxlen/
    path = "."
    file_name = "maxlen.txt"
    file_result = FileResult.new(path, file_name, :text, 0, 0)
    line_num = 1
    match_start_index = 53
    match_end_index = 59
    line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
    lines_before = []
    lines_after = []
    search_result = SearchResult.new(pattern, file_result, line_num, line, lines_before, lines_after, match_start_index, match_end_index)

    settings = SearchSettings.new(colorize: false, max_line_length: 100)
    formatter = SearchResultFormatter.new(settings)

    expected_line = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
    expected_output = "./maxlen.txt: #{line_num}: [#{match_start_index}:#{match_end_index}]: #{expected_line}"


    output = SearchResultFormatter.format(formatter, search_result)
    assert output == expected_output
  end

  test "single-line search_result longer than max_line_length colorize" do
    pattern = ~r/maxlen/
    path = "."
    file_name = "maxlen.txt"
    file_result = FileResult.new(path, file_name, :text, 0, 0)
    line_num = 1
    match_start_index = 53
    match_end_index = 59
    line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
    lines_before = []
    lines_after = []
    search_result = SearchResult.new(pattern, file_result, line_num, line, lines_before, lines_after, match_start_index, match_end_index)

    settings = SearchSettings.new(colorize: true, max_line_length: 100)
    formatter = SearchResultFormatter.new(settings)

    expected_line = "...89012345678901234567890123456789012345678901#{ConsoleColor.green}maxlen#{ConsoleColor.reset}89012345678901234567890123456789012345678901..."
    expected_output = "./maxlen.txt: #{line_num}: [#{match_start_index}:#{match_end_index}]: #{expected_line}"

    output = SearchResultFormatter.format(formatter, search_result)
    assert output == expected_output
  end

  test "search_result match longer than max_line_length colorize" do
    pattern = ~r/\d+maxlen\d+/
    path = "."
    file_name = "maxlen.txt"
    file_result = FileResult.new(path, file_name, :text, 0, 0)
    line_num = 1
    match_start_index = 1
    match_end_index = 110
    line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
    lines_before = []
    lines_after = []
    search_result = SearchResult.new(pattern, file_result, line_num, line, lines_before, lines_after, match_start_index, match_end_index)

    settings = SearchSettings.new(colorize: true, max_line_length: 100)
    formatter = SearchResultFormatter.new(settings)

    expected_line = "#{ConsoleColor.green}0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456#{ConsoleColor.reset}..."
    expected_output = "./maxlen.txt: #{line_num}: [#{match_start_index}:#{match_end_index}]: #{expected_line}"

    output = SearchResultFormatter.format(formatter, search_result)
    assert output == expected_output
  end

  test "search_result 2 match longer than max_line_length colorize" do
    pattern = ~r/\d+maxlen\d+/
    path = "."
    file_name = "maxlen.txt"
    file_result = FileResult.new(path, file_name, :text, 0, 0)
    line_num = 1
    match_start_index = 11
    match_end_index = 120
    line = "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ"
    lines_before = []
    lines_after = []
    search_result = SearchResult.new(pattern, file_result, line_num, line, lines_before, lines_after, match_start_index, match_end_index)

    settings = SearchSettings.new(colorize: true, max_line_length: 100)
    formatter = SearchResultFormatter.new(settings)

    expected_line = "...#{ConsoleColor.green}3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456#{ConsoleColor.reset}..."
    expected_output = "./maxlen.txt: #{line_num}: [#{match_start_index}:#{match_end_index}]: #{expected_line}"

    output = SearchResultFormatter.format(formatter, search_result)
    assert output == expected_output
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
    formatter = SearchResultFormatter.new(settings)
    search_result_formatted = SearchResultFormatter.format(formatter, search_result)
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
