#!/usr/bin/env ruby
# frozen_string_literal: true

################################################################################
#
# rbsearch.rb
#
# A ruby implementation of xsearch
#
################################################################################

require 'rbfind/common'
require 'rbfind/filetypes'
require 'rbfind/fileutil'

require_relative 'rbsearch/searcher'
require_relative 'rbsearch/searchoption'
require_relative 'rbsearch/searchoptions'
require_relative 'rbsearch/searchresult'
require_relative 'rbsearch/searchresultformatter'
require_relative 'rbsearch/searchsettings'

def search_main
  options = RbSearch::SearchOptions.new

  settings =
    begin
      options.search_settings_from_args(ARGV)
    rescue RbSearch::SearchError => e
      handle_search_error(e, true, options)
    end

  RbFind.log("settings: #{settings}") if settings.debug

  if settings.print_usage
    RbFind.log("\n")
    options.usage
  end

  if settings.print_version
    RbFind.log("Version: #{RbSearch::VERSION}")
    abort
  end

  search(options, settings)
end

def handle_search_error(err, colorize, options)
  RbFind.log('')
  RbFind::log_err("#{err.message}\n", colorize)
  options.usage
end

def search(options, settings)
  searcher =
    begin
      RbSearch::Searcher.new(settings)
    rescue RbSearch::SearchError => e
      handle_search_error(e, settings.colorize, options)
    rescue RbFind::FindError => e
      handle_search_error(e, settings.colorize, options)
    rescue => e
      handle_search_error(e, settings.colorize, options)
    end
  search_results = searcher.search
  formatter = RbSearch::SearchResultFormatter.new(settings)

  # print the results
  if settings.print_results
    RbFind.log("\n")
    searcher.print_search_results(search_results, formatter)
  end

  if settings.print_dirs
    RbFind.log("\n")
    searcher.print_matching_dirs(search_results, formatter)
  end

  if settings.print_files
    RbFind.log("\n")
    searcher.print_matching_files(search_results, formatter)
  end

  if settings.print_lines
    RbFind.log("\n")
    searcher.print_matching_lines(search_results, formatter)
  end

rescue RuntimeError => e
  handle_search_error(e, settings.colorize, options)
end
