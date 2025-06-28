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

  def colorize_line(line, start_idx, end_idx) do
    before_text = String.slice(line, 0, start_idx - 1)
    match_text = String.slice(line, start_idx - 1, end_idx - start_idx)
    after_text = String.slice(line, end_idx - 1, String.length(line))
    colorized = IO.ANSI.green() <> match_text <> IO.ANSI.reset()
    before_text <> colorized <> after_text
  end

  def format_lines(lines, start_line_num, max_line_num) do
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
        format_lines(search_result.lines_before, search_result.line_num - length(search_result.lines_before), max_line_num)
        |> Enum.each(fn line -> IO.write(out, "#{line}\n") end)
      end
      colorized_line =
        if formatter.settings.colorize do
          FileResultFormatter.colorize(search_result.line, search_result.match_start_index, search_result.match_end_index)
        else
          search_result.line
        end
      IO.write(out, "> #{search_result.line_num} | #{colorized_line}\n")
      if search_result.lines_after != [] do
        format_lines(search_result.lines_after, search_result.line_num + 1, max_line_num)
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
        line =
          if formatter.settings.colorize do
            FileResultFormatter.colorize(search_result.line, search_result.match_start_index - 1, search_result.match_end_index - 1)
          else
            search_result.line
          end
        ": #{search_result.line_num}: #{match_range}: #{String.trim_leading(line)}"
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
