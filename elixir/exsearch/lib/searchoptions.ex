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

defmodule ExSearch.SearchOptionsLoader do
  @moduledoc """
  Documentation for `ExSearch.SearchOptionsLoader`.
  """

  def load_options() do
    # Load the search options from the searchoptions.json file.
    search_options_path = ExSearch.Config.search_options_path
    {:ok, json} = File.read(search_options_path)
    search_options = JSON.decode!(json)
    search_options["searchoptions"]
    |> Enum.map(fn o -> ExSearch.SearchOption.new([short_arg: Map.get(o, "short", ""), long_arg: o["long"], description: o["desc"]]) end)
  end
end

defmodule ExSearch.SearchOptions do
  @moduledoc """
  Documentation for `ExSearch.SearchOptions`.
  """

  alias ExFind.ArgTokenizer
  alias ExFind.FileTypes
  alias ExFind.SortBy
  alias ExFind.StringUtil

  alias ExSearch.SearchError
  alias ExSearch.SearchSettings

  require OptionParser

  defstruct options: ExSearch.SearchOptionsLoader.load_options()

  def new(), do: __struct__()

  defp bool_arg_action_map() do
    %{
      allmatches: fn b, settings -> %{settings | first_match: not b} end,
      archivesonly: fn b, settings -> SearchSettings.set_archives_only(settings, b) end,
      colorize: fn b, settings -> %{settings | colorize: b} end,
      debug: fn b, settings -> SearchSettings.set_debug(settings, b) end,
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
      noprintmatches: fn b, settings -> %{settings | print_matches: not b} end,
      noprintresults: fn b, settings -> %{settings | print_results: not b} end,
      noprintusage: fn b, settings -> %{settings | print_usage: not b} end,
      norecursive: fn b, settings -> %{settings | recursive: not b} end,
      nosearcharchives: fn b, settings -> %{settings | search_archives: not b} end,
      printdirs: fn b, settings -> %{settings | print_dirs: b} end,
      printfiles: fn b, settings -> %{settings | print_files: b} end,
      printlines: fn b, settings -> %{settings | print_lines: b} end,
      printmatches: fn b, settings -> %{settings | print_matches: b} end,
      printresults: fn b, settings -> %{settings | print_results: b} end,
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

  defp get_arg_tokenizer(options, arg_action_maps) do
    {bool_arg_action_map, int_arg_action_map, str_arg_action_map} = arg_action_maps
    %ArgTokenizer{
      options: options,
      bool_opts: Map.keys(bool_arg_action_map),
      int_opts: Map.keys(int_arg_action_map),
      str_opts: Map.keys(str_arg_action_map)
    }
  end

  def update_settings_from_json(settings, json, arg_tokenizer, arg_action_maps) do
    case ArgTokenizer.tokenize_json(json, arg_tokenizer) do
      {:ok, tokens} -> update_settings_from_tokens(settings, tokens, arg_tokenizer, arg_action_maps)
      {:error, message} -> {:error, message}
    end
  end

  def get_settings_from_json(json, options) do
    arg_action_maps = arg_action_maps()
    arg_tokenizer = get_arg_tokenizer(options, arg_action_maps)
    update_settings_from_json(SearchSettings.new(), json, arg_tokenizer, arg_action_maps)
  end

  def get_settings_from_json!(json, options) do
    case get_settings_from_json(json, options) do
      {:error, message} -> raise SearchError, message: message
      {:ok, settings} -> settings
    end
  end

  def update_settings_from_file(settings, json_file, arg_tokenizer, arg_action_maps) do
    case ArgTokenizer.tokenize_file(json_file, arg_tokenizer) do
      {:ok, tokens} -> update_settings_from_tokens(settings, tokens, arg_tokenizer, arg_action_maps)
      {:error, message} -> {:error, message}
    end
  end

  def get_settings_from_file(json_file, options) do
    arg_action_maps = arg_action_maps()
    arg_tokenizer = get_arg_tokenizer(options, arg_action_maps)
    case update_settings_from_file(SearchSettings.new(), json_file, arg_tokenizer, arg_action_maps) do
      {:error, "Unable to parse JSON"} -> {:error, "Unable to parse JSON in settings file: #{json_file}"}
      {:error, message} -> {:error, message}
      {:ok, settings} -> {:ok, settings}
    end
  end

  def get_settings_from_file!(json_file, options) do
    case get_settings_from_file(json_file, options) do
      {:error, message} -> raise SearchError, message: message
      {:ok, settings} -> settings
    end
  end

  def update_settings_from_tokens!(settings, tokens, arg_tokenizer, arg_action_maps) do
    {bool_arg_action_map, int_arg_action_map, str_arg_action_map} = arg_action_maps
    case tokens do
      [] -> settings
      [t | ts] ->
        case t.arg_type do
          :boolean ->
            k = t.name
            v = t.value
            cond do
              Map.has_key?(bool_arg_action_map, k) ->
                update_settings_from_tokens!(Map.get(bool_arg_action_map, k).(v, settings), ts, arg_tokenizer, arg_action_maps)
              true -> raise SearchError, message: "Invalid option: #{k}"
            end
          :integer ->
            k = t.name
            v = t.value
            cond do
              Map.has_key?(int_arg_action_map, k) ->
                update_settings_from_tokens!(Map.get(int_arg_action_map, k).(v, settings), ts, arg_tokenizer, arg_action_maps)
              true -> raise SearchError, message: "Invalid option: #{k}"
            end
          :string ->
            k = t.name
            v = t.value
            cond do
              Map.has_key?(str_arg_action_map, k) ->
                update_settings_from_tokens!(Map.get(str_arg_action_map, k).(v, settings), ts, arg_tokenizer, arg_action_maps)
              k == :settings_file -> case update_settings_from_file(settings, v, arg_tokenizer, arg_action_maps) do
                {:ok, new_settings} -> update_settings_from_tokens!(new_settings, ts, arg_tokenizer, arg_action_maps)
                {:error, message} -> raise SearchError, message: message
              end
              true -> raise SearchError, message: "Invalid option: #{k}"
            end
          :unknown ->
            raise SearchError, message: "Invalid option: #{t.name}"
        end
    end
  end

  def update_settings_from_tokens(settings, tokens, arg_tokenizer, arg_action_maps) do
    try do
      {:ok, update_settings_from_tokens!(settings, tokens, arg_tokenizer, arg_action_maps)}
    rescue
      e in SearchError -> {:error, e.message}
    end
  end

  def update_settings_from_args!(settings, args, options) do
    arg_action_maps = arg_action_maps()
    arg_tokenizer = get_arg_tokenizer(options, arg_action_maps)
    case ArgTokenizer.tokenize_args(args, arg_tokenizer) do
      {:ok, tokens} -> update_settings_from_tokens!(settings, tokens, arg_tokenizer, arg_action_maps)
      {:error, message} -> raise SearchError, message: message
    end
  end

  def update_settings_from_args(settings, args, options) do
    try do
      {:ok, update_settings_from_args!(settings, args, options)}
    rescue
      e in SearchError -> {:error, e.message}
    end
  end

  def get_settings_from_args(args, options) do
    settings = SearchSettings.new([print_results: true])
    update_settings_from_args(settings, args, options)
  end

  def get_settings_from_args!(args, options) do
    case get_settings_from_args(args, options) do
      {:error, message} -> raise SearchError, message: message
      {:ok, settings} -> settings
    end
  end

  defp get_usage_string(options) do
    opt_strings = options
                  |> Enum.sort(fn o1, o2 -> ExSearch.SearchOption.sort_arg(o1) <= ExSearch.SearchOption.sort_arg(o2) end)
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

  def usage(options) do
    IO.puts(get_usage_string(options))
  end

end
