defmodule ExSearchTest.SearchSettingsTest do
  alias ExFind.StringUtil
  alias ExSearch.SearchSettings
  use ExUnit.Case
  doctest ExSearch.SearchSettings

  test "default settings" do
    settings = SearchSettings.new()
    assert settings.archives_only == false
    assert settings.colorize == true
    assert settings.debug == false
    assert settings.in_archive_extensions == []
    assert settings.in_archive_file_patterns == []
    assert settings.in_dir_patterns == []
    assert settings.in_extensions == []
    assert settings.in_file_patterns == []
    assert settings.in_file_types == []
    assert settings.in_lines_after_patterns == []
    assert settings.in_lines_before_patterns == []
    assert settings.include_hidden == false
    assert settings.lines_after == 0
    assert settings.lines_before == 0
    assert settings.max_depth == -1
    assert settings.max_last_mod == nil
    assert settings.max_line_length == 150
    assert settings.max_size == 0
    assert settings.min_depth == -1
    assert settings.min_last_mod == nil
    assert settings.min_size == 0
    assert settings.out_archive_extensions == []
    assert settings.out_archive_file_patterns == []
    assert settings.out_dir_patterns == []
    assert settings.out_extensions == []
    assert settings.out_file_patterns == []
    assert settings.out_file_types == []
    assert settings.out_lines_after_patterns == []
    assert settings.out_lines_before_patterns == []
    assert settings.paths == []
    assert settings.print_dirs == false
    assert settings.print_files == false
    assert settings.print_lines == false
    assert settings.print_results == false
    assert settings.print_usage == false
    assert settings.print_version == false
    assert settings.recursive == true
    assert settings.search_archives == false
    assert settings.search_patterns == []
    assert settings.sort_by == :file_path
    assert settings.sort_case_insensitive == false
    assert settings.sort_descending == false
    assert settings.text_file_encoding == :utf8
    assert settings.unique_lines == false
    assert settings.verbose == false
  end

  test "add an extension" do
    settings = SearchSettings.new()
    settings = SearchSettings.add_extensions(settings, "ex", :in_extensions)
    assert settings.in_extensions == ["ex"]
  end

  test "add comma-delimited extensions" do
    settings = SearchSettings.new()
    settings = SearchSettings.add_extensions(settings, "ex,exs", :in_extensions)
    assert settings.in_extensions == ["ex", "exs"]
  end

  test "add list of extensions" do
    settings = SearchSettings.new()
    settings = SearchSettings.add_extensions(settings, ["ex", "exs"], :in_extensions)
    assert settings.in_extensions == ["ex", "exs"]
  end

  test "add a file pattern" do
    settings = SearchSettings.new()
    settings = SearchSettings.add_pattern(settings, "find", :in_file_patterns)
    assert settings.in_file_patterns == [~r/find/]
  end

  test "add a search pattern" do
    settings = SearchSettings.new()
    settings = SearchSettings.add_pattern(settings, "search", :search_patterns)
    assert settings.search_patterns == [~r/search/]
  end

  test "add an invalid pattern" do
    settings = SearchSettings.new()
    assert_raise ExFind.FindError, fn ->
      _ = SearchSettings.add_patterns(settings, ["file", "search)"], :in_file_patterns)
    end
  end

  test "add list of patterns" do
    settings = SearchSettings.new()
    settings = SearchSettings.add_patterns(settings, ["search", "file"], :in_file_patterns)
    assert settings.in_file_patterns == [~r/search/, ~r/file/]
  end

  test "set archives_only sets include_archives" do
    settings = SearchSettings.new()
    settings = SearchSettings.set_archives_only(settings, true)
    assert settings.archives_only == true
    assert settings.search_archives == true
  end

  test "set debug sets verbose" do
    settings = SearchSettings.new()
    settings = SearchSettings.set_debug(settings, true)
    assert settings.debug == true
    assert settings.verbose == true
  end

  test "convert date string to datetime instance" do
    date_str = "2024-06-01"
    datetime = StringUtil.to_datetime(date_str)
    assert datetime == ~U[2024-06-01 00:00:00Z]
  end

  test "convert datetime string to datetime instance" do
    datetime_str = "2024-06-01T09:30:00Z"
    datetime = StringUtil.to_datetime(datetime_str)
    assert datetime == ~U[2024-06-01 09:30:00Z]
  end

end
