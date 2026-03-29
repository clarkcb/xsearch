defmodule ExSearch.Config do
  @moduledoc """
  Documentation for `ExSearch.Config`.
  """

  @xsearch_path System.get_env("XSEARCH_PATH") || Path.join([System.user_home(), "src", "xsearch"])
  @shared_path Path.join([@xsearch_path, "shared"])
  @search_options_path Path.join([@shared_path, "searchoptions.json"])
  @default_search_settings_path Path.join([System.user_home(), ".config", "xsearch", "settings.json"])

  @version "0.1.0"

  def xsearch_path, do: @xsearch_path

  def shared_path, do: @shared_path

  def search_options_path, do: @search_options_path

  def default_search_settings_path, do: @default_search_settings_path

  def version, do: @version

end
