defmodule ExSearch.SearchConfig do
  @moduledoc """
  Documentation for `ExSearch.SearchConfig`.
  """

  alias ExFind.FindConfig

  @default_xsearch_config_dir Path.join([System.user_home(), ".config", "xsearch"])
  @default_xsearch_path Path.join([System.user_home(), "src", "xsearch"])
  @shared_name "shared"
  @search_options_name "searchoptions.json"
  @version "0.1.0"

  def get_xsearch_config_dir() do
    System.get_env("XSEARCH_CONFIG_DIR") || @default_xsearch_config_dir
  end

  def get_default_search_settings_path() do
    xsearch_config_dir = get_xsearch_config_dir()
    Path.join([xsearch_config_dir, "settings.json"])
  end

  def get_xsearch_path() do
    System.get_env("XSEARCH_PATH") || @default_xsearch_path
  end

  def get_search_options_path() do
    xsearch_path = get_xsearch_path()
    shared_path = Path.join([xsearch_path, @shared_name])
    Path.join([shared_path, @search_options_name])
  end

  defstruct [:find_config, :xsearch_path, :search_options_path, :default_search_settings_path, :version]

  def new() do
    __struct__([
      find_config: FindConfig.new(),
      xsearch_path: get_xsearch_path(),
      search_options_path: get_search_options_path(),
      default_search_settings_path: get_default_search_settings_path(),
      version: @version
    ])
  end
end
