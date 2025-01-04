alias ExFind.FindOptions

defmodule ExSearchTest.SearchOptionsTest do
  alias ExSearch.SearchOptions
  use ExUnit.Case
  doctest ExSearch.SearchOptions

  test "no args" do
    search_options = SearchOptions.new()
    {:ok, settings} = SearchOptions.get_settings_from_args([], search_options.options)
    assert settings.archives_only == false
    assert settings.debug == false
    assert settings.colorize == true
    assert settings.in_archive_extensions == []
    assert settings.in_archive_file_patterns == []
    assert settings.in_dir_patterns == []
    assert settings.in_extensions == []
    assert settings.in_file_patterns == []
    assert settings.in_file_types == []
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
    assert settings.paths == []
    assert settings.print_dirs == false
    assert settings.print_files == false
    assert settings.print_lines == false
    assert settings.print_results == true
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

  test "valid args" do
    search_options = SearchOptions.new()
    {:ok, settings} = SearchOptions.get_settings_from_args(["-x", "ex,exs", "src", "-f", "find"], search_options.options)
    assert settings.in_extensions == ["ex", "exs"]
    assert settings.paths == ["src"]
    assert settings.in_file_patterns == [~r/find/]
  end

  test "set archives_only" do
    search_options = SearchOptions.new()
    {:ok, settings} = SearchOptions.get_settings_from_args(["--archivesonly"], search_options.options)
    assert settings.archives_only == true
    assert settings.search_archives == true
  end

  test "set debug" do
    search_options = SearchOptions.new()
    {:ok, settings} = SearchOptions.get_settings_from_args(["--debug"], search_options.options)
    assert settings.debug == true
    assert settings.verbose == true
  end

  test "settings from json" do
    json = """
    {
      "in-ext": ["ex", "exs"],
      "path": ["~/src/xfind/elixir/exfind"],
      "out-dirpattern": ["dep"]
      "out-filepattern": ["test"],
      "debug": true,
      "includehidden": true,
    }
    """
    {status, settings} = FindOptions.get_settings_from_json(json)
    assert status == :ok
    assert settings.in_extensions == ["ex", "exs"]
    assert settings.paths == ["~/src/xfind/elixir/exfind"]
    assert settings.out_dir_patterns == [~r/dep/]
    assert settings.out_file_patterns == [~r/test/]
    assert settings.debug == true
    assert settings.include_hidden == true
  end

  test "settings from invalid json" do
    json = """
    {
      "in-ext": ["ex", "exs",
      "path": ["~/src/xfind/elixir/exfind"],
      "out-dirpattern": ["dep"]
      "out-filepattern": ["test"],
      "debug": true,
      "includehidden": true,
    }
    """
    {status, _value} = FindOptions.get_settings_from_json(json)
    assert status == :error
  end

  test "settings! from json" do
    json = """
    {
      "in-ext": ["ex", "exs"],
      "path": ["~/src/xfind/elixir/exfind"],
      "out-dirpattern": ["dep"]
      "out-filepattern": ["test"],
      "debug": true,
      "includehidden": true,
    }
    """
    settings = FindOptions.get_settings_from_json!(json)
    assert settings.in_extensions == ["ex", "exs"]
    assert settings.paths == ["~/src/xfind/elixir/exfind"]
    assert settings.out_dir_patterns == [~r/dep/]
    assert settings.out_file_patterns == [~r/test/]
    assert settings.debug == true
    assert settings.include_hidden == true
  end

  test "settings! from invalid json" do
    json = """
    {
      "in-ext": ["ex", "exs",
      "path": ["~/src/xfind/elixir/exfind"],
      "out-dirpattern": ["dep"]
      "out-filepattern": ["test"],
      "debug": true,
      "includehidden": true,
    }
    """
    assert_raise ExFind.FindError, fn ->
      _ = FindOptions.get_settings_from_json!(json)
    end
  end

  test "settings from non-existent file" do
    json_file = "/non/existent/file.json"
    {status, _value} = FindOptions.get_settings_from_file(json_file)
    assert status == :error
  end
end
