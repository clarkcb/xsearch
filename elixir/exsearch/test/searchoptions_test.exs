alias ExFind.FindOptions

defmodule ExSearchTest.SearchOptionsTest do
  alias ExSearch.SearchConfig
  alias ExSearch.SearchOptions
  use ExUnit.Case
  doctest ExSearch.SearchOptions

  test "no args" do
    config = SearchConfig.new()
    search_options = SearchOptions.new(config)
    {:ok, settings} = SearchOptions.get_settings_from_args(search_options, [])
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
    config = SearchConfig.new()
    search_options = SearchOptions.new(config)
    {:ok, settings} = SearchOptions.get_settings_from_args(search_options, ["-x", "ex,exs", "src", "-f", "find"])
    assert settings.in_extensions == ["ex", "exs"]
    assert settings.paths == ["src"]
    assert Enum.map(settings.in_file_patterns, fn p -> p.source end) == ["find"]
  end

  test "set archives_only" do
    config = SearchConfig.new()
    search_options = SearchOptions.new(config)
    {:ok, settings} = SearchOptions.get_settings_from_args(search_options, ["--archivesonly"])
    assert settings.archives_only == true
    assert settings.search_archives == true
  end

  test "set debug" do
    config = SearchConfig.new()
    search_options = SearchOptions.new(config)
    {:ok, settings} = SearchOptions.get_settings_from_args(search_options, ["--debug"])
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
    config = SearchConfig.new()
    search_options = SearchOptions.new(config)
    {status, settings} = SearchOptions.get_settings_from_json(search_options, json)
    assert status == :ok
    assert settings.in_extensions == ["ex", "exs"]
    assert settings.paths == ["~/src/xfind/elixir/exfind"]
    assert Enum.map(settings.out_dir_patterns, fn p -> p.source end) == ["dep"]
    assert Enum.map(settings.out_file_patterns, fn p -> p.source end) == ["test"]
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
    config = SearchConfig.new()
    search_options = SearchOptions.new(config)
    {status, _value} = SearchOptions.get_settings_from_json(search_options, json)
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
    config = SearchConfig.new()
    search_options = SearchOptions.new(config)
    settings = SearchOptions.get_settings_from_json!(search_options, json)
    assert settings.in_extensions == ["ex", "exs"]
    assert settings.paths == ["~/src/xfind/elixir/exfind"]
    assert Enum.map(settings.out_dir_patterns, fn p -> p.source end) == ["dep"]
    assert Enum.map(settings.out_file_patterns, fn p -> p.source end) == ["test"]
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
    config = SearchConfig.new()
    search_options = SearchOptions.new(config)
    assert_raise ExSearch.SearchError, fn ->
      _ = SearchOptions.get_settings_from_json!(search_options, json)
    end
  end

  test "settings from non-existent file" do
    json_file = "/non/existent/file.json"
    config = SearchConfig.new()
    search_options = SearchOptions.new(config)
    {status, _value} = SearchOptions.get_settings_from_file(search_options, json_file)
    assert status == :error
  end
end
