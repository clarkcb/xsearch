#!/usr/bin/env ruby
################################################################################
#
# rbsearch.rb
#
# A file search utility implemented in ruby
#
################################################################################

require 'searcher.rb'
require 'searchoptions.rb'

DEBUG = false

def log(message)
  puts message
end

def main
  if DEBUG
    log("ARGV(#{ARGV.count}): #{ARGV.join(", ")}")
  end

  searchoptions = SearchOptions.new

  settings = nil
  begin
    settings = searchoptions.search_settings_from_args(ARGV)
  rescue ArgumentError => e
    log("\nArgumentError: #{e.message}\n\n")
    searchoptions.usage
  end

  settings.debug = settings.debug || DEBUG

  if settings.debug
    log("settings:\n#{settings.to_s}")
  end

  if settings.printusage
    searchoptions.usage
  end

  if settings.printversion
    log('Version: 0.1')
    abort
  end

  begin
    searcher = Searcher.new(settings)
    searcher.search

    # print the results
    if settings.printresults
      log("\n")
      searcher.print_results()
    end

    if settings.listdirs
      log("\n")
      dirs = searcher.get_matching_dirs()
      if not dirs.empty?
        log("Directories with matches (#{dirs.count}):")
        dirs.each do |d|
          log("#{d}\n")
        end
      end
    end

    if settings.listfiles
      log("\n")
      files = searcher.get_matching_files()
      if not files.empty?
        log("Files with matches (#{files.count}):")
        files.each do |f|
          log("#{f}\n")
        end
      end
    end

    if settings.listlines
      log("\n")
      lines = searcher.get_matching_lines()
      if not lines.empty?
        log("Lines with matches (#{lines.count}):")
        lines.each do |line|
          log("#{line}\n")
        end
      end
    end

  rescue RuntimeError => e
    log("\nRuntimeError: #{e.message}\n\n")
    searchoptions.usage
  end

end

if __FILE__ == $0
  main
end
