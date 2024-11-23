defmodule ExSearchTest.SearcherTest do
  alias ExSearch.Config
  alias ExSearch.Searcher
  alias ExSearch.SearchSettings
  use ExUnit.Case
  doctest ExSearch.Searcher

    test "search lines" do
      file_path = Path.join([Config.shared_path, "testFiles", "testFile2.txt"])
      settings = SearchSettings.new([paths: [file_path]])
                 |> SearchSettings.add_patterns(["Searcher"], :search_patterns)

      searcher = Searcher.new(settings)

      [first_result, second_result] = Searcher.search!(searcher)

      assert first_result.line_num == 30
      assert first_result.match_start_index == 3
      assert first_result.match_end_index == 11

      assert second_result.line_num == 36
      assert second_result.match_start_index == 24
      assert second_result.match_end_index == 32

    end

  end
