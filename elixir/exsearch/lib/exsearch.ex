defmodule ExSearch.Main do
  @moduledoc """
  Documentation for `ExSearch.Main`.
  """

  alias ExFind.Logging
  alias ExSearch.Searcher
  alias ExSearch.SearchError
  alias ExSearch.SearchOptions
  alias ExSearch.SearchResultFormatter

  def handle_error(message, colorize, search_options) do
    Logging.log_error("\nERROR: #{message}", colorize)
    SearchOptions.usage(search_options.options)
  end

  def handle_results(results, settings) do
    formatter = SearchResultFormatter.new(settings)
    if settings.print_results do
      Searcher.print_results(results, formatter)
    end
    if settings.print_dirs do
      Searcher.print_dirs(results, formatter)
    end
    if settings.print_files do
      Searcher.print_files(results, formatter)
    end
    if settings.print_lines do
      Searcher.print_lines(results, formatter)
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
      searcher = Searcher.new(settings)
      case Searcher.search(searcher) do
        {:error, message} -> handle_error(message, settings.colorize, search_options)
        {:ok, results} -> handle_results(results, settings)
      end
    end
  end

  def main(args) do
    search_options = SearchOptions.new()
    try do
      case SearchOptions.get_settings_from_args(args, search_options.options) do
        {:error, message} -> handle_error(message, true, search_options)
        {:ok, settings} -> search(settings, search_options)
      end
    rescue
      e in SearchError -> handle_error(e.message, true, search_options)
    end
  end
end
