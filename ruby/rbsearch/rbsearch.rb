#!/usr/bin/env ruby
# frozen_string_literal: true

################################################################################
#
# rbsearch.rb
#
# A file search utility implemented in ruby
#
################################################################################

require_relative 'common.rb'
require_relative 'searcher.rb'
require_relative 'searchoptions.rb'

def main
  options = SearchOptions.new

  settings =
    begin
      options.search_settings_from_args(ARGV)
    rescue SearchError => e
      handle_error(e, options)
    end

  log("settings: #{settings}") if settings.debug

  if settings.printusage
    log("\n")
    options.usage
  end

  if settings.printversion
    log('Version: 0.1')
    abort
  end

  search(options, settings)
end

def handle_error(err, options)
  log("\nERROR: #{err.message}\n\n")
  options.usage
end

def search(options, settings)
  searcher = Searcher.new(settings)
  searcher.search

  # print the results
  if settings.printresults
    log("\n")
    searcher.print_results
  end

  if settings.listdirs
    log("\n")
    dirs = searcher.get_matching_dirs
    log("Directories with matches (#{dirs.size}):")
    dirs.each do |d|
      log("#{d}\n")
    end
  end

  if settings.listfiles
    log("\n")
    files = searcher.get_matching_files
    log("Files with matches (#{files.size}):")
    files.each do |f|
      log("#{f}\n")
    end
  end

  if settings.listlines
    log("\n")
    lines = searcher.get_matching_lines
    hdr_text =
      if settings.uniquelines
        'Unique lines with matches'
      else
        'Lines with matches'
      end
    log("#{hdr_text} (#{lines.size}):")
    lines.each do |line|
      log("#{line}\n")
    end
  end

rescue SearchError => e
  handle_error(e, options)

rescue RuntimeError => e
  handle_error(e, options)
end

if __FILE__ == $PROGRAM_NAME
  main
end
