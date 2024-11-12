defmodule ExSearch.SearchSettings do
  @moduledoc """
  Documentation for `ExSearch.SearchSettings`.
  """

  alias ExFind.FindSettings
  alias ExSearch.SearchError

  defstruct archives_only: false,
            colorize: true,
            debug: false,
            first_match: false,
            follow_symlinks: false,
            in_archive_extensions: [],
            in_archive_file_patterns: [],
            in_dir_patterns: [],
            in_extensions: [],
            in_file_patterns: [],
            in_file_types: [],
            in_lines_after_patterns: [],
            in_lines_before_patterns: [],
            include_hidden: false,
            lines_after: 0,
            lines_after_to_patterns: [],
            lines_after_until_patterns: [],
            lines_before: 0,
            max_depth: -1,
            max_last_mod: nil,
            max_line_length: 150,
            max_size: 0,
            min_depth: -1,
            min_last_mod: nil,
            min_size: 0,
            multi_line_search: false,
            out_archive_extensions: [],
            out_archive_file_patterns: [],
            out_dir_patterns: [],
            out_extensions: [],
            out_file_patterns: [],
            out_file_types: [],
            out_lines_after_patterns: [],
            out_lines_before_patterns: [],
            paths: [],
            print_dirs: false,
            print_files: false,
            print_lines: false,
            print_results: false,
            print_usage: false,
            print_version: false,
            recursive: true,
            search_archives: false,
            search_patterns: [],
            sort_by: :file_path,
            sort_case_insensitive: false,
            sort_descending: false,
            text_file_encoding: :utf8,
            unique_lines: false,
            verbose: false

  def to_find_settings(settings) do
    FindSettings.new(
      archives_only: settings.archives_only,
      debug: settings.debug,
      follow_symlinks: settings.follow_symlinks,
      in_archive_extensions: settings.in_archive_extensions,
      in_archive_file_patterns: settings.in_archive_file_patterns,
      in_dir_patterns: settings.in_dir_patterns,
      in_extensions: settings.in_extensions,
      in_file_patterns: settings.in_file_patterns,
      in_file_types: settings.in_file_types,
      include_archives: settings.search_archives,
      include_hidden: settings.include_hidden,
      max_depth: settings.max_depth,
      max_last_mod: settings.max_last_mod,
      max_size: settings.max_size,
      min_depth: settings.min_depth,
      min_last_mod: settings.min_last_mod,
      min_size: settings.min_size,
      out_archive_extensions: settings.out_archive_extensions,
      out_archive_file_patterns: settings.out_archive_file_patterns,
      out_dir_patterns: settings.out_dir_patterns,
      out_extensions: settings.out_extensions,
      out_file_patterns: settings.out_file_patterns,
      out_file_types: settings.out_file_types,
      paths: settings.paths,
      print_dirs: settings.print_dirs,
      print_files: settings.print_files,
      print_usage: settings.print_usage,
      print_version: settings.print_version,
      recursive: settings.recursive,
      sort_by: settings.sort_by,
      sort_case_insensitive: settings.sort_case_insensitive,
      sort_descending: settings.sort_descending,
      verbose: settings.verbose
    )
  end

  def new(), do: __struct__()
  def new(args), do: __struct__(args)

  def add_extensions(settings, extensions, extensions_name) do
    FindSettings.add_extensions(settings, extensions, extensions_name)
  end

  def add_extension(settings, extension, extensions_name) do
    FindSettings.add_extension(settings, extension, extensions_name)
  end

  def add_patterns(settings, patterns, patterns_name) do
    FindSettings.add_patterns(settings, patterns, patterns_name)
  end

  def add_pattern(settings, pattern, patterns_name) do
    FindSettings.add_pattern(settings, pattern, patterns_name)
  end

  def need_size?(settings) do
    FindSettings.need_size?(settings)
  end

  def need_last_mod?(settings) do
    FindSettings.need_last_mod?(settings)
  end

  def set_archives_only(settings, archives_only) do
    search_archives = archives_only || settings.search_archives
    %{settings | archives_only: archives_only, search_archives: search_archives}
  end

  def set_debug(settings, debug) do
    FindSettings.set_debug(settings, debug)
  end

  def set_encoding(settings, encoding) when is_atom(encoding) do
    if Enum.member?([:utf8, :latin1, :unicode, :utf16, :utf32], encoding) do
      %{settings | text_file_encoding: encoding}
    else
      raise SearchError, message: "Invalid encoding: #{encoding}"
    end
  end

  def set_encoding(settings, encoding) when is_binary(encoding) do
    set_encoding(settings, String.replace(encoding, ~r/\W+/, "") |> String.to_atom())
  end

end
