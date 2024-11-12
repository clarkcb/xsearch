defmodule ExSearch.SearchOption do
  @moduledoc """
  Documentation for `ExSearch.SearchOption`.
  """

  defstruct short_arg: "", long_arg: "", description: ""

  def new(args), do: __struct__(args)

  def sort_arg(option) do
    if option.short_arg == "" do
      option.long_arg
    else
      "#{String.downcase(option.short_arg)}a#{option.long_arg}"
    end
  end

  def to_arg_string(option) do
    if option.short_arg == "" do
      "--#{option.long_arg}"
    else
      "-#{option.short_arg},--#{option.long_arg}"
    end
  end
end

defmodule ExSearch.SearchOptionsUtils do
  @moduledoc """
  Documentation for `ExSearch.SearchOptionsUtils`.
  """

  def load_options() do
    # Load the search options from the searchoptions.json file.
    search_options_path = ExSearch.Config.search_options_path
    {:ok, json} = File.read(search_options_path)
    search_options = JSON.decode!(json)
    search_options["searchoptions"]
    |> Enum.map(fn o -> ExSearch.SearchOption.new([short_arg: Map.get(o, "short", ""), long_arg: o["long"], description: o["desc"]]) end)
    |> Enum.sort(fn o1, o2 -> ExSearch.SearchOption.sort_arg(o1) <= ExSearch.SearchOption.sort_arg(o2) end)
  end
end


defmodule ExSearch.SearchOptions do
  @moduledoc """
  Documentation for `ExSearch.SearchOptions`.
  """

  alias ExFind.FileTypes
  alias ExFind.FindOptions
  alias ExFind.SortBy
  alias ExFind.StringUtil

  alias ExSearch.SearchError
  alias ExSearch.SearchSettings

  require OptionParser

  defstruct options: ExSearch.SearchOptionsUtils.load_options()

  def new(), do: __struct__()

  defp bool_arg_action_map() do
    %{
      allmatches: fn b, settings -> %{settings | first_match: not b} end,
      archivesonly: fn b, settings -> SearchSettings.set_archives_only(settings, b) end,
      colorize: fn b, settings -> %{settings | colorize: b} end,
      debug: fn b, settings -> SearchSettings.set_debug(settings, b) end,
      excludearchives: fn b, settings -> %{settings | include_archives: not b} end,
      excludehidden: fn b, settings -> %{settings | include_hidden: not b} end,
      firstmatch: fn b, settings -> %{settings | first_match: b} end,
      followsymlinks: fn b, settings -> %{settings | follow_symlinks: b} end,
      help: fn b, settings -> %{settings | print_usage: b} end,
      includehidden: fn b, settings -> %{settings | include_hidden: b} end,
      multilinesearch: fn b, settings -> %{settings | multi_line_search: b} end,
      nocolorize: fn b, settings -> %{settings | colorize: not b} end,
      nofollowsymlinks: fn b, settings -> %{settings | follow_symlinks: not b} end,
      noprintdirs: fn b, settings -> %{settings | print_dirs: not b} end,
      noprintfiles: fn b, settings -> %{settings | print_files: not b} end,
      noprintlines: fn b, settings -> %{settings | print_lines: not b} end,
      noprintmatches: fn b, settings -> %{settings | print_results: not b} end,
      noprintusage: fn b, settings -> %{settings | print_usage: not b} end,
      norecursive: fn b, settings -> %{settings | recursive: not b} end,
      nosearcharchives: fn b, settings -> %{settings | search_archives: not b} end,
      printdirs: fn b, settings -> %{settings | print_dirs: b} end,
      printfiles: fn b, settings -> %{settings | print_files: b} end,
      printlines: fn b, settings -> %{settings | print_lines: b} end,
      printmatches: fn b, settings -> %{settings | print_results: b} end,
      printusage: fn b, settings -> %{settings | print_usage: b} end,
      printversion: fn b, settings -> %{settings | print_version: b} end,
      recursive: fn b, settings -> %{settings | recursive: b} end,
      searcharchives: fn b, settings -> %{settings | search_archives: b} end,
      sort_ascending: fn b, settings -> %{settings | sort_descending: not b} end,
      sort_caseinsensitive: fn b, settings -> %{settings | sort_case_insensitive: b} end,
      sort_casesensitive: fn b, settings -> %{settings | sort_case_insensitive: not b} end,
      sort_descending: fn b, settings -> %{settings | sort_descending: b} end,
      uniquelines: fn b, settings -> %{settings | unique_lines: b} end,
      verbose: fn b, settings -> %{settings | verbose: b} end,
      version: fn b, settings -> %{settings | print_version: b} end
    }
  end

  defp int_arg_action_map() do
    %{
      linesafter: fn i, settings -> %{settings | lines_after: i} end,
      linesbefore: fn i, settings -> %{settings | lines_before: i} end,
      maxdepth: fn i, settings -> %{settings | max_depth: i} end,
      maxlinelength: fn i, settings -> %{settings | max_line_length: i} end,
      maxsize: fn i, settings -> %{settings | max_size: i} end,
      mindepth: fn i, settings -> %{settings | min_depth: i} end,
      minsize: fn i, settings -> %{settings | min_size: i} end
    }
  end

  defp str_arg_action_map() do
    %{
      encoding: fn s, settings -> SearchSettings.set_encoding(settings, s) end,
      in_archiveext: fn s, settings -> SearchSettings.add_extensions(settings, s, :in_archive_extensions) end,
      in_archivefilepattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :in_archive_file_patterns) end,
      in_dirpattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :in_dir_patterns) end,
      in_ext: fn s, settings -> SearchSettings.add_extensions(settings, s, :in_extensions) end,
      in_filepattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :in_file_patterns) end,
      in_filetype: fn s, settings -> %{settings | in_file_types: settings.in_file_types ++ [FileTypes.get_file_type_for_name(s)]} end,
      in_linesafterpattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :in_lines_after_patterns) end,
      in_linesbeforepattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :in_lines_before_patterns) end,
      linesaftertopattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :lines_after_to_patterns) end,
      linesafteruntilpattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :lines_after_until_patterns) end,
      maxlastmod: fn s, settings -> %{settings | max_last_mod: StringUtil.to_datetime(s)} end,
      minlastmod: fn s, settings -> %{settings | min_last_mod: StringUtil.to_datetime(s)} end,
      out_archiveext: fn s, settings -> SearchSettings.add_extensions(settings, s, :out_archive_extensions) end,
      out_archivefilepattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :out_archive_file_patterns) end,
      out_dirpattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :out_dir_patterns) end,
      out_ext: fn s, settings -> SearchSettings.add_extensions(settings, s, :out_extensions) end,
      out_filepattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :out_file_patterns) end,
      out_filetype: fn s, settings -> %{settings | out_file_types: settings.out_file_types ++ [FileTypes.get_file_type_for_name(s)]} end,
      out_linesafterpattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :out_lines_after_patterns) end,
      out_linesbeforepattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :out_lines_before_patterns) end,
      path: fn s, settings -> %{settings | paths: settings.paths ++ [s]} end,
      searchpattern: fn s, settings -> SearchSettings.add_pattern(settings, s, :search_patterns) end,
      sort_by: fn s, settings -> %{settings | sort_by: SortBy.get_sort_by_for_name(s)} end
    }
  end

  defp arg_action_maps() do
    {bool_arg_action_map(), int_arg_action_map(), str_arg_action_map()}
  end

  def get_settings_from_args(args, options) do
    settings = SearchSettings.new([print_results: true])
    arg_action_maps = arg_action_maps()
    {parsed_args, paths, invalid} = FindOptions.parse_args(args, options, arg_action_maps)
    {updated_args, updated_paths} =
      if Enum.member?(paths, "-1") do
        {parsed_args ++ [{:firstmatch, true}], paths -- ["-1"]}
      else
        {parsed_args, paths}
      end
    updated_args_with_paths = updated_args ++ Enum.map(updated_paths, fn p -> {:path, p} end)
    case invalid do
      [{opt, nil} | _rest] -> {:error, "Invalid option: #{opt}"}
      [] -> {:ok, FindOptions.update_settings_from_args(settings, updated_args_with_paths, arg_action_maps)}
    end
  end

  def get_settings_from_args!(args, options) do
    case get_settings_from_args(args, options) do
      {:error, message} -> raise SearchError, message: message
      {:ok, settings} -> settings
    end
  end

  def get_settings_from_json(json) do
    FindOptions.update_settings_from_json(SearchSettings.new(), json)
  end

  def get_settings_from_json!(json) do
    case get_settings_from_json(json) do
      {:error, message} -> raise SearchError, message: message
      {:ok, settings} -> settings
    end
  end

  def get_settings_from_file(json_file) do
    case File.read(json_file) do
      {:ok, json} -> get_settings_from_json(json)
      {:error, e} -> {:error, e}
    end
  end

  def usage(options) do
    IO.puts(get_usage_string(options))
  end

  defp get_usage_string(options) do
    opt_strings = options
                  |> Enum.map(fn o -> {ExSearch.SearchOption.to_arg_string(o), o.description} end)
    longest = Enum.map(opt_strings, fn {opt, _} -> String.length(opt) end) |> Enum.max()
    opt_lines = opt_strings
                |> Enum.map(fn {opt, desc} -> " #{opt}#{String.duplicate(" ", longest - String.length(opt))}  #{desc}" end)
    """
    \nUsage:
     exsearch [options] -s <searchpattern> <path> [<path> ...]

    Options:
    #{Enum.join(opt_lines, "\n")}
    """
  end

end
