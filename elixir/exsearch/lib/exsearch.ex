defmodule ExSearch.Main do
  @moduledoc """
  Documentation for `ExSearch.Main`.
  """

  alias ExFind.FileResult
  alias ExSearch.Searcher
  alias ExSearch.SearchError
  alias ExSearch.SearchOptions
  alias ExSearch.SearchResultFormatter
  alias ExFind.Logging

  def handle_error(message, search_options) do
    Logging.log_error("\nERROR: #{message}")
    SearchOptions.usage(search_options.options)
  end

  def get_dirs(results) do
    if results == [] do
      []
    else
      Enum.map(results, fn r -> r.file.path end) |> Enum.uniq() |> Enum.sort()
    end
  end

  def print_dirs(results) do
    dirs = get_dirs(results)
    if dirs == [] do
      Logging.log("\nMatching directories: 0")
    else
      Logging.log("\nMatching directories (#{Enum.count(dirs)}):\n#{Enum.join(dirs, "\n")}")
    end
  end

  def get_files(results) do
    if results == [] do
      []
    else
      Enum.map(results, fn r -> FileResult.to_string(r.file) end) |> Enum.uniq()
    end
  end

  def print_files(results) do
    files = get_files(results)
    if files == [] do
      Logging.log("\nMatching files: 0")
    else
      Logging.log("\nMatching files (#{Enum.count(files)}):\n#{Enum.join(files, "\n")}")
    end
  end

  def get_lines(results, settings) do
    if results == [] do
      []
    else
      lines = Enum.map(results, fn r -> String.trim_leading(r.line) end)
      if settings.unique_lines do
        lines |> Enum.uniq()
      else
        lines
      end
    end
  end

  def print_lines(results, settings) do
    lines = get_lines(results, settings)
    cond do
      lines == [] -> Logging.log("\nMatching lines: 0")
      settings.unique_lines -> Logging.log("\nUnique matching lines (#{Enum.count(lines)}):\n#{Enum.join(lines, "\n")}")
      true -> Logging.log("\nMatching lines (#{Enum.count(lines)}):\n#{Enum.join(lines, "\n")}")
    end
  end

  def print_results(results, settings) do
    if results == [] do
      Logging.log("\nSearch results: 0")
    else
      result_strs = Enum.map(results, fn r -> SearchResultFormatter.format(r, settings) end)
      Logging.log("\nSearch results (#{Enum.count(result_strs)}):\n#{Enum.join(result_strs, "\n")}")
    end
  end

  def handle_results(results, settings) do
    if settings.print_results do
      print_results(results, settings)
    end
    if settings.print_dirs do
      print_dirs(results)
    end
    if settings.print_files do
      print_files(results)
    end
    if settings.print_lines do
      print_lines(results, settings)
    end
  end

  def search(settings, search_options) do
    if settings.debug do
      Logging.log("\nsettings: #{inspect(settings)}\n")
    end

    if settings.print_usage or settings.print_version do
      if settings.print_usage do
        SearchOptions.usage(search_options.options)
      else
        Logging.log("\nExSearch version: #{ExSearch.Config.version()}")
      end
    else
      searcher = Searcher.new([file_types: ExFind.FileTypes.new(), settings: settings])
      case Searcher.search(searcher) do
        {:error, message} -> handle_error(message, search_options)
        {:ok, results} -> handle_results(results, settings)
      end
    end
  end

  def main(args) do
    search_options = SearchOptions.new()
    try do
      case SearchOptions.get_settings_from_args(args, search_options.options) do
        {:error, message} -> handle_error(message, search_options)
        {:ok, settings} -> search(settings, search_options)
      end
    rescue
      e in SearchError -> handle_error(e.message, search_options)
    end
  end
end
