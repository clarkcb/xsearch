defmodule ExSearch.Searcher do
  @moduledoc """
  Documentation for `ExSearch.Searcher`.
  """

  alias ExFind.FileResult
  alias ExFind.Finder
  alias ExFind.StringUtil

  alias ExSearch.SearchError
  alias ExSearch.SearchResult
  alias ExSearch.SearchSettings

  defstruct [:settings]

  def new(settings) do
    __struct__([settings: settings])
  end

  # ----------------------------------------------------------------------------
  # Binary search (blob search)
  # ----------------------------------------------------------------------------
  def blob_match_index_to_search_result(pattern, match_index) do
    {match_idx, match_length} = match_index
    SearchResult.new(pattern, nil, 0, "", [], [], match_idx + 1, match_idx + match_length + 1)
  end

  def search_blob_for_pattern(searcher, blob, pattern) do
    matching_indices =
      if searcher.settings.first_match do
        Regex.run(pattern, blob, return: :index)
      else
        Regex.scan(pattern, blob, return: :index) |> Enum.map(fn i -> List.first(i) end)
      end
    case matching_indices do
      nil -> []
      [] -> []
      _ ->
        Enum.map(matching_indices, fn i -> blob_match_index_to_search_result(pattern, i) end)
    end
  end

  def search_blob(searcher, blob) do
    searcher.settings.search_patterns
    |> Enum.map(fn p -> search_blob_for_pattern(searcher, blob, p) end)
    |> List.flatten()
  end

  def search_binary_file(searcher, file_result) do
    case File.open(FileResult.relative_path(file_result), [:binary, :read]) do
      {:error, message} -> {:error, message}
      {:ok, file} ->
        search_results = search_blob(searcher, IO.binread(file, :eof))
        Enum.map(search_results, fn sr -> %{sr | file: file_result} end)
    end
  end

  # ----------------------------------------------------------------------------
  # Text-based search (multi-line search)
  #
  # This search method is extremely slow and I haven't pinpointed the reason yet,
  # so for now it's disabled.
  # ----------------------------------------------------------------------------
  def text_match_index_to_search_result(searcher, text, pattern, match_index, start_line_indices, end_line_indices) do
    {match_idx, match_length} = match_index
    prev_start_line_indices = Enum.take_while(start_line_indices, fn i -> i < match_idx end)
    line_num = length(prev_start_line_indices)
    prev_end_line_indices = Enum.take(end_line_indices, line_num)
    line_start_index = List.last(prev_start_line_indices)
    line_end_index = List.last(prev_end_line_indices)
    line = String.slice(text, line_start_index, line_end_index - line_start_index)
    lines_before =
      if searcher.settings.lines_before > 0 do
        before_start_indices = Enum.drop(prev_start_line_indices, -1)
                               |> Enum.take(searcher.settings.lines_before * -1)
        before_end_indices = Enum.drop(prev_end_line_indices, -1)
                             |> Enum.take(searcher.settings.lines_before * -1)
        Enum.zip(before_start_indices, before_end_indices)
        |> Enum.map(fn {s, e} -> String.slice(text, s, e - s) end)
      else
        []
      end
    lines_after =
      if searcher.settings.lines_after > 0 do
        after_start_indices = Enum.drop(start_line_indices, line_num)
                              |> Enum.take(searcher.settings.lines_after)
        after_end_indices = Enum.drop(end_line_indices, line_num)
                            |> Enum.take(searcher.settings.lines_after)
        Enum.zip(after_start_indices, after_end_indices)
        |> Enum.map(fn {s, e} -> String.slice(text, s, e - s) end)
      else
        []
      end
    SearchResult.new(pattern, nil, line_num, line, lines_before, lines_after, match_idx - line_start_index + 1, match_idx - line_start_index + match_length + 1)
  end

  def search_text_for_pattern(searcher, text, pattern, start_line_indices, end_line_indices) do
    matching_indices =
      if searcher.settings.first_match do
        Regex.run(pattern, text, return: :index)
      else
        Regex.scan(pattern, text, return: :index) |> Enum.map(fn i -> List.first(i) end)
      end
    if matching_indices != nil and matching_indices != [] do
      IO.puts("matching_indices: #{inspect(matching_indices)}")
    end
    case matching_indices do
      nil -> []
      [] -> []
      _ ->
        Enum.map(matching_indices, fn i -> text_match_index_to_search_result(searcher, text, pattern, i, start_line_indices, end_line_indices) end)
    end
  end

  def search_text(searcher, text) do
    charlist = String.to_charlist(text)
    new_line_indices = Enum.to_list(0..length(charlist) - 1)
                       |> Enum.reduce([], fn i, acc -> if Enum.at(charlist, i) == 10, do: acc ++ [i], else: acc end)
    start_line_indices = [0] ++ Enum.map(new_line_indices, fn i -> i + 1 end)
    end_line_indices = new_line_indices ++ [String.length(text)]
    searcher.settings.search_patterns
    |> Enum.map(fn p -> search_text_for_pattern(searcher, text, p, start_line_indices, end_line_indices) end)
    |> List.flatten()
  end

  def search_text_file_contents(searcher, file_result) do
    case File.open(FileResult.relative_path(file_result), [{:encoding, searcher.settings.text_file_encoding}]) do
      {:error, message} -> {:error, message}
      {:ok, file} ->
        search_results = search_text(searcher, IO.read(file, :eof))
        Enum.map(search_results, fn sr -> %{sr | file: file_result} end)
    end
  end

  # ----------------------------------------------------------------------------
  # Line-based search
  # ----------------------------------------------------------------------------
  def line_match_index_to_search_result(line_num, line, lines_before, lines_after, pattern, match_index) do
    {match_idx, match_length} = match_index
    SearchResult.new(pattern, nil, line_num, String.trim_trailing(line), Enum.map(lines_before, &(String.trim_trailing(&1))),
                     Enum.map(lines_after, &(String.trim_trailing(&1))), match_idx + 1, match_idx + match_length + 1)
  end

  def search_line_for_pattern(searcher, line_num, line, pattern, lines_before, lines_after) do
    # first filter on lines_before and lines_after
    if (!Enum.empty?(searcher.settings.in_lines_before_patterns)
        and !StringUtil.any_matches_any_pattern(lines_before, searcher.settings.in_lines_before_patterns))
        or (!Enum.empty?(searcher.settings.out_lines_before_patterns)
            and StringUtil.any_matches_any_pattern(lines_before, searcher.settings.out_lines_before_patterns))
        or (!Enum.empty?(searcher.settings.in_lines_after_patterns)
            and !StringUtil.any_matches_any_pattern(lines_after, searcher.settings.in_lines_after_patterns))
        or (!Enum.empty?(searcher.settings.out_lines_after_patterns)
            and StringUtil.any_matches_any_pattern(lines_after, searcher.settings.out_lines_after_patterns)) do
      []
    else
      matching_indices =
        if searcher.settings.first_match do
          Regex.run(pattern, line, return: :index)
        else
          Regex.scan(pattern, line, return: :index) |> Enum.map(fn i -> List.first(i) end)
        end
      case matching_indices do
        nil -> []
        [] -> []
        _ ->
          Enum.map(matching_indices, fn i -> line_match_index_to_search_result(line_num, line, lines_before, lines_after, pattern, i) end)
      end
    end
  end

  def line_stream_to_results(searcher, line_stream, lines_before, results) do
    if searcher.settings.first_match and results != [] do
      results
    else
      case Enum.take(line_stream, 1) do
        [] -> results
        [{line, line_num}] ->
          lines_after =
            if searcher.settings.lines_after > 0 do
              Enum.drop(line_stream, 1)
              |> Enum.take(searcher.settings.lines_after)
              |> Enum.map(fn {l, _ln} -> l end)
            else
              []
            end
          line_results =
            searcher.settings.search_patterns
            |> Enum.map(fn p -> search_line_for_pattern(searcher, line_num, line, p, lines_before, lines_after) end)
            |> List.flatten()
          new_lines_before =
            if searcher.settings.lines_before > 0 do
              if length(lines_before) < searcher.settings.lines_before do
                lines_before ++ [line]
              else
                Enum.drop(lines_before, 1) ++ [line]
              end
            else
              []
            end
          line_stream_to_results(searcher, Enum.drop(line_stream, 1), new_lines_before, results ++ line_results)
      end
    end
  end

  def search_line_stream(searcher, stream) do
    line_stream =
      stream
      |> Stream.with_index(1)
   line_stream_to_results(searcher, line_stream, [], [])
  end

  def search_text_file_lines(searcher, file_result) do
    search_results =
      search_line_stream(searcher, File.stream!(FileResult.relative_path(file_result), [encoding: searcher.settings.text_file_encoding]))
      |> Enum.to_list()
    Enum.map(search_results, fn sr -> %{sr | file: file_result} end)
  end

  def search_text_file(searcher, file_result) do
    if searcher.settings.multi_line_search do
      # Contents searching is disabled for now, too slow
      # search_text_file_contents(searcher, file_result)
      search_text_file_lines(searcher, file_result)
    else
      search_text_file_lines(searcher, file_result)
    end
  end

  def search_file(searcher, file_result) do
    case file_result.file_type do
      :archive -> []
      :binary -> search_binary_file(searcher, file_result)
      t ->
        if Enum.member?([:code, :xml, :text], t), do: search_text_file(searcher, file_result), else: []
    end
  end

  def search_files(searcher, file_results) do
    search_results = Enum.map(file_results, fn fr -> search_file(searcher, fr) end)
    |> List.flatten()
    {:ok, search_results}
  end

  def search_files_async(searcher, file_results) do
    tasks = Enum.map(file_results, fn fr -> Task.async(fn -> search_file(searcher, fr) end) end)
    search_results = Task.await_many(tasks) |> List.flatten()
    {:ok, search_results}
  end

  def search(searcher) do
    # temporary for turning on/off async search
    do_async = true
    case validate_settings(searcher.settings) do
      {:error, message} -> {:error, message}
      {:ok, _} ->
        find_settings = SearchSettings.to_find_settings(searcher.settings)
        finder = Finder.new(find_settings)
        case Finder.find(finder) do
          {:error, message} -> {:error, message}
          {:ok, file_results} ->
            if do_async do
              search_files_async(searcher, file_results)
            else
              search_files(searcher, file_results)
            end
        end
    end
  end

  def search!(searcher) do
    case search(searcher) do
      {:error, message} -> raise SearchError, message: message
      {:ok, results} -> results
    end
  end

  def validate_settings(settings) do
    case Finder.validate_settings(settings) do
      {:error, message} -> {:error, message}
      {:ok, _} ->
        cond do
          Enum.empty?(settings.search_patterns) ->
            {:error, "No search patterns defined"}
          settings.lines_after < 0 ->
            {:error, "Invalid linesafter"}
          settings.lines_before < 0 ->
            {:error, "Invalid linesbefore"}
          settings.max_line_length < 0 ->
            {:error, "Invalid maxlinelength"}
          true -> {:ok, "Settings are valid"}
        end
    end
  end

end
