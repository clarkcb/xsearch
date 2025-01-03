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

def print_results(results, settings)
  formatter = RbSearch::SearchResultFormatter.new(settings)
  RbFind.log("Search results (#{results.size}):")
  results.each do |r|
    RbFind.log(formatter.format(r))
  end
end

def print_result(search_result, settings)
  s = ''
  s += "#{search_result.pattern}: " if settings.search_patterns.size > 1
  s += search_result.to_s
  RbFind.log(s)
end

def get_matching_dirs(results)
  results.map { |r| r.file.dir_name }.uniq.sort
end

def get_matching_files(results)
  results.map { |r| r.file.to_s }.uniq.sort
end

def get_matching_lines(results, settings)
  lines = results.map { |r| r.line.strip }.sort { |l1, l2| l1.upcase <=> l2.upcase }
  lines.uniq! if settings.unique_lines
  lines
end

def search(options, settings)
  searcher = RbSearch::Searcher.new(settings)
  results = searcher.search

  # print the results
  if settings.print_results
    RbFind.log("\n")
    print_results(results, settings)
  end

  if settings.print_dirs
    RbFind.log("\n")
    dirs = get_matching_dirs(results)
    if dirs.empty?
      RbFind.log("\nMatching directories: 0")
    else
      RbFind.log("\nMatching directories (#{dirs.size}):")
      dirs.each do |d|
        RbFind.log("#{d}\n")
      end
    end
  end

  if settings.print_files
    RbFind.log("\n")
    files = get_matching_files(results)
    if files.empty?
      RbFind.log("\nMatching files: 0")
    else
      RbFind.log("\nMatching files (#{files.size}):")
      files.each do |f|
        RbFind.log("#{f}\n")
      end
    end
  end

  if settings.print_lines
    RbFind.log("\n")
    lines = get_matching_lines(results, settings)
    hdr_text =
      if settings.unique_lines
        'Unique lines with matches'
      else
        'Lines with matches'
      end
    if lines.empty?
      RbFind.log("#{hdr_text}: 0")
    else
      RbFind.log("#{hdr_text} (#{lines.size}):")
      lines.each do |line|
        RbFind.log("#{line}\n")
      end
    end
  end

rescue RbSearch::SearchError => e
  handle_error(e, options)

rescue RuntimeError => e
  handle_error(e, options)
end
