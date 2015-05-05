#!/usr/bin/env ruby
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
  searchoptions = SearchOptions.new

  settings = nil
  begin
    settings = searchoptions.search_settings_from_args(ARGV)
  rescue ArgumentError => e
    log("\nArgumentError: #{e.message}\n\n")
    searchoptions.usage
  end

  settings.debug = settings.debug

  if settings.debug
    log("settings:\n#{settings.to_s}")
  end

  if settings.printusage
    log("\n")
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
      log("Directories with matches (#{dirs.count}):")
      dirs.each do |d|
        log("#{d}\n")
      end
    end

    if settings.listfiles
      log("\n")
      files = searcher.get_matching_files()
      log("Files with matches (#{files.count}):")
      files.each do |f|
        log("#{f}\n")
      end
    end

    if settings.listlines
      log("\n")
      lines = searcher.get_matching_lines()
      hdrText = "Lines with matches"
      if settings.uniquelines
        hdrText = "Unique lines with matches"
      end
      log("#{hdrText} (#{lines.count}):")
      lines.each do |line|
        log("#{line}\n")
      end
    end

  rescue RuntimeError => e
    log("\nERROR: #{e.message}\n\n")
    searchoptions.usage
  end

end

if __FILE__ == $0
  main
end
