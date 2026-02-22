defmodule ExSearch.SearchResult do
  @moduledoc """
  Documentation for `ExFind.FileResult`.
  """

  defstruct pattern: "", file: nil, line_num: 0, line: "", lines_before: [], lines_after: [], match_start_index: 0, match_end_index: 0

  def new(pattern, file, line_num, line, lines_before, lines_after, match_start_index, match_end_index) do
    __struct__(
      [
        pattern: pattern,
        file: file,
        line_num: line_num,
        line: line,
        lines_before: lines_before,
        lines_after: lines_after,
        match_start_index: match_start_index,
        match_end_index: match_end_index
      ]
    )
  end

  def new(args), do: __struct__(args)
end

defmodule ExSearch.SearchResultFormatter do
  @moduledoc """
  Documentation for `ExSearch.SearchResultFormatter`.
  """

  alias ExFind.FileResultFormatter

  defstruct [:settings, :file_formatter]

  def new(settings) do
    __struct__([
      settings: settings,
      file_formatter: FileResultFormatter.new(settings)
    ])
  end

  def format_result_match(formatter, search_result) do
    match_start_idx = search_result.match_start_index - 1
    match_end_idx = search_result.match_end_index - 1
    match_length = match_end_idx - match_start_idx

    {prefix, suffix, color_start_idx, color_end_idx, match_start_idx, match_end_idx} =
      if match_length > formatter.settings.max_line_length do
        prefix = if match_start_idx > 2, do: "...", else: ""
        suffix = "..."
        color_start_idx = String.length(prefix)
        color_end_idx = formatter.settings.max_line_length - 3
        match_end_idx = match_start_idx + color_end_idx
        match_start_idx = match_start_idx + color_start_idx
        {prefix, suffix, color_start_idx, color_end_idx, match_start_idx, match_end_idx}
      else
        {"", "", 0, match_end_idx, match_start_idx, match_end_idx}
      end

    match_string = prefix <> String.slice(search_result.line, match_start_idx, match_end_idx - match_start_idx) <> suffix
    if formatter.settings.colorize do
      FileResultFormatter.colorize(match_string, color_start_idx, color_end_idx, formatter.settings.line_color)
    else
      match_string
    end
  end

  def get_indices_for_max_line_length(line_start_idx, line_end_idx, match_start_idx, match_end_idx, line_length, max_line_length) do
    if (line_end_idx - line_start_idx) < max_line_length do
      {lsi, msi, mei} =
        if line_start_idx > 0 do
          {line_start_idx - 1, match_start_idx + 1, match_end_idx + 1}
        else
          {line_start_idx, match_start_idx, match_end_idx}
        end
      lei =
        if (line_end_idx - lsi) < max_line_length && line_end_idx < line_length do
          line_end_idx + 1
        else
          line_end_idx
        end
      get_indices_for_max_line_length(lsi, lei, msi, mei, line_length, max_line_length)
    else
      {line_start_idx, line_end_idx, match_start_idx, match_end_idx}
    end
  end

  def format_result_line(formatter, search_result) do
    if (search_result.line == "" || formatter.settings.max_line_length == 0) do
      ""
    else
      if formatter.settings.max_line_length > 0 && (search_result.match_end_index - search_result.match_start_index) > formatter.settings.max_line_length do
        format_result_match(formatter, search_result)
      else
        line_len = String.length(search_result.line)
        line_start_idx = line_len - String.length(String.trim_leading(search_result.line))
        line_end_idx = line_len - 1 - (line_len - String.length(String.trim_trailing(search_result.line)))
        match_length = search_result.match_end_index - search_result.match_start_index
        match_start_idx = search_result.match_start_index - 1 - line_start_idx
        match_end_idx = match_start_idx + match_length
        trimmed_length = line_end_idx - line_start_idx
        {line_start_idx, line_end_idx, match_start_idx, match_end_idx} =
          if formatter.settings.max_line_length > 0 && trimmed_length > formatter.settings.max_line_length do
            get_indices_for_max_line_length(search_result.match_start_index - 1, search_result.match_end_index - 1, 0, match_length, trimmed_length, formatter.settings.max_line_length)
          else
            {line_start_idx, line_end_idx + 1, match_start_idx, match_end_idx}
          end
        {prefix, line_start_idx} = if line_start_idx > 2, do: {"...", line_start_idx + 3}, else: {"", line_start_idx}
        {suffix, line_end_idx} = if line_end_idx < (trimmed_length - 2), do: {"...", line_end_idx - 3}, else: {"", line_end_idx}
        formatted = prefix <> String.slice(search_result.line, line_start_idx, line_end_idx - line_start_idx) <> suffix
        if formatter.settings.colorize do
          FileResultFormatter.colorize(formatted, match_start_idx, match_end_idx, formatter.settings.line_color)
        else
          formatted
        end
      end
    end
  end

  def format_lines_with_nums(lines, start_line_num, max_line_num) do
    line_num_padding = String.length("#{max_line_num}")
    lines_with_line_nums = Enum.with_index(lines, start_line_num)
    Enum.map(lines_with_line_nums, fn {line, line_num} ->
      line_num_str = String.pad_leading("#{line_num}", line_num_padding + 2, " ")
      "#{line_num_str} | #{line}"
    end)
  end

  def multi_line_format(formatter, search_result) do
    max_line_num = search_result.line_num + length(search_result.lines_after)
    case StringIO.open("", [], fn out ->
      match_index_range = "[#{search_result.match_start_index}:#{search_result.match_end_index}]"
      file_path = FileResultFormatter.format_file_result(formatter.file_formatter, search_result.file)
      file_path_line = "#{file_path}: #{search_result.line_num}: #{match_index_range}"
      IO.write(out, String.duplicate("=", 80) <> "\n")
      IO.write(out, "#{file_path_line}\n")
      IO.write(out, String.duplicate("-", 80) <> "\n")
      if search_result.lines_before != [] do
        format_lines_with_nums(search_result.lines_before, search_result.line_num - length(search_result.lines_before), max_line_num)
        |> Enum.each(fn line -> IO.write(out, "#{line}\n") end)
      end
      matching_line =
        if formatter.settings.colorize do
          FileResultFormatter.colorize(search_result.line, search_result.match_start_idx - 1, search_result.match_end_idx - 1, formatter.settings.line_color)
        else
          search_result.line
        end
      IO.write(out, "> #{search_result.line_num} | #{matching_line}\n")
      if search_result.lines_after != [] do
        format_lines_with_nums(search_result.lines_after, search_result.line_num + 1, max_line_num)
        |> Enum.each(fn line -> IO.write(out, "#{line}\n") end)
      end
      StringIO.contents(out)
    end) do
      {:ok, {"", multi_line_result}} -> multi_line_result
      _ -> ""
    end
  end

  def single_line_format(formatter, search_result) do
    match_range = "[#{search_result.match_start_index}:#{search_result.match_end_index}]"
    line_result = case {search_result.line_num, search_result.line} do
      {0, ""} -> " matches at #{match_range}"
      _ ->
        line = format_result_line(formatter, search_result)
        ": #{search_result.line_num}: #{match_range}: #{line}"
    end
    FileResultFormatter.format_file_result(formatter.file_formatter, search_result.file) <> line_result
  end

  def format(formatter, search_result) do
    if formatter.settings.lines_before > 0 or formatter.settings.lines_after > 0 do
      multi_line_format(formatter, search_result)
    else
      single_line_format(formatter, search_result)
    end
  end
end

defmodule ExSearch.SearchResultSorter do
  @moduledoc """
  Documentation for `ExSearch.SearchResultSorter`.
  """

  def get_file_path_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {String.downcase(r.file.path), String.downcase(r.file.name), r.line_num, r.match_start_index, r.match_end_index} end
    else
      fn r -> {r.file.path, r.file.name, r.line_num, r.match_start_index, r.match_end_index} end
    end
  end

  def get_file_name_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {String.downcase(r.file.name), String.downcase(r.file.path), r.line_num, r.match_start_index, r.match_end_index} end
    else
      fn r -> {r.file.name, r.file.path, r.line_num, r.match_start_index, r.match_end_index} end
    end
  end

  def get_file_size_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {r.file.file_size, String.downcase(r.file.path), String.downcase(r.file.name), r.line_num, r.match_start_index, r.match_end_index} end
    else
      fn r -> {r.file.file_size, r.file.path, r.file.name, r.line_num, r.match_start_index, r.match_end_index} end
    end
  end

  def get_file_type_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {r.file.file_type, String.downcase(r.file.path), String.downcase(r.file.name), r.line_num, r.match_start_index, r.match_end_index} end
    else
      fn r -> {r.file.file_type, r.file.path, r.file.name, r.line_num, r.match_start_index, r.match_end_index} end
    end
  end

  def get_last_mod_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {r.file.last_mod, String.downcase(r.file.path), String.downcase(r.file.name), r.line_num, r.match_start_index, r.match_end_index} end
    else
      fn r -> {r.file.last_mod, r.file.path, r.file.name, r.line_num, r.match_start_index, r.match_end_index} end
    end
  end

  def get_search_result_mapper(settings) do
    case settings.sort_by do
      :file_name -> get_file_name_mapper(settings)
      :file_size -> get_file_size_mapper(settings)
      :file_type -> get_file_type_mapper(settings)
      :last_mod  -> get_last_mod_mapper(settings)
      _          -> get_file_path_mapper(settings)
    end
  end

  def sort(settings, results) do
    search_result_mapper = get_search_result_mapper(settings)
    direction = if settings.sort_descending, do: :desc, else: :asc
    Enum.sort_by(results, search_result_mapper, direction)
  end
end
