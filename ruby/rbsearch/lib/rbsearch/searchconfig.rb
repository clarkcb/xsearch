# frozen_string_literal: true

require 'rbfind/findconfig'

module RbSearch

  # SearchConfig - basic config info
  class SearchConfig < RbFind::FindConfig
    attr_reader :search_options_path
    attr_reader :default_search_settings_path

    def initialize
      super
      @search_options_path = File.realpath(File.join(File.dirname(__FILE__), "../../data/searchoptions.json"))
      default_xsearch_config_dir = File.join(ENV['HOME'], '.config', 'xsearch')
      xsearch_config_dir = ENV.fetch('XSEARCH_CONFIG_DIR', default_xsearch_config_dir)
      @default_search_settings_path = File.join(xsearch_config_dir, 'settings.json')
    end
  end
end
