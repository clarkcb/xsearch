#!/usr/bin/env ruby
# frozen_string_literal: true

################################################################################
#
# rbsearch.rb
#
# A ruby implementation of xsearch
#
################################################################################

require_relative 'rbsearch/common'
require_relative 'rbsearch/filetypes'
require_relative 'rbsearch/fileutil'
require_relative 'rbsearch/searcher'
require_relative 'rbsearch/searchfile'
require_relative 'rbsearch/searchoption'
require_relative 'rbsearch/searchoptions'
require_relative 'rbsearch/searchresult'
require_relative 'rbsearch/searchsettings'

def main
  options = RbSearch::SearchOptions.new

  settings =
    begin
      options.search_settings_from_args(ARGV)
    rescue RbSearch::SearchError => e
      handle_error(e, options)
    end

  RbSearch::log("settings: #{settings}") if settings.debug

  if settings.printusage
    RbSearch::log("\n")
    options.usage
  end

  if settings.printversion
    RbSearch::log("Version: #{RbSearch::VERSION}")
    abort
  end

  search(options, settings)
end

def handle_error(err, options)
  RbSearch::log("\nERROR: #{err.message}\n\n")
  options.usage
end

def search(options, settings)
  searcher = RbSearch::Searcher.new(settings)
  searcher.search

  # print the results
  if settings.printresults
    RbSearch::log("\n")
    searcher.print_results
  end

  if settings.listdirs
    RbSearch::log("\n")
    dirs = searcher.get_matching_dirs
    RbSearch::log("Directories with matches (#{dirs.size}):")
    dirs.each do |d|
      RbSearch::log("#{d}\n")
    end
  end

  if settings.listfiles
    RbSearch::log("\n")
    files = searcher.get_matching_files
    RbSearch::log("Files with matches (#{files.size}):")
    files.each do |f|
      RbSearch::log("#{f}\n")
    end
  end

  if settings.listlines
    RbSearch::log("\n")
    lines = searcher.get_matching_lines
    hdr_text =
      if settings.uniquelines
        'Unique lines with matches'
      else
        'Lines with matches'
      end
    RbSearch::log("#{hdr_text} (#{lines.size}):")
    lines.each do |line|
      RbSearch::log("#{line}\n")
    end
  end

rescue RbSearch::SearchError => e
  handle_error(e, options)

rescue RuntimeError => e
  handle_error(e, options)
end
