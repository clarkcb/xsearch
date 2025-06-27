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

# require_relative 'rbsearch/common'
# require_relative 'rbsearch/filetypes'
# require_relative 'rbsearch/fileutil'
require_relative 'rbsearch/searcher'
require_relative 'rbsearch/searchoption'
require_relative 'rbsearch/searchoptions'
require_relative 'rbsearch/searchresult'
require_relative 'rbsearch/searchresultformatter'
require_relative 'rbsearch/searchsettings'

def main
  options = RbSearch::SearchOptions.new

  settings =
    begin
      options.search_settings_from_args(ARGV)
    rescue RbSearch::SearchError => e
      handle_error(e, options)
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

def handle_error(err, options)
  RbFind.log("\nERROR: #{err.message}\n\n")
  options.usage
end

def search(options, settings)
  searcher = RbSearch::Searcher.new(settings)
  results = searcher.search
  formatter = RbSearch::SearchResultFormatter.new(settings)

  # print the results
  if settings.print_results
    RbFind.log("\n")
    searcher.print_search_results(results, formatter)
  end

  if settings.print_dirs
    RbFind.log("\n")
    searcher.print_matching_dirs(results, formatter)
  end

  if settings.print_files
    RbFind.log("\n")
    searcher.print_matching_files(results, formatter)
  end

  if settings.print_lines
    RbFind.log("\n")
    searcher.print_matching_lines(results, formatter)
  end

rescue RbSearch::SearchError => e
  handle_error(e, options)

rescue RuntimeError => e
  handle_error(e, options)
end
