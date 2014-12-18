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

def main
  if DEBUG
    puts "ARGV(#{ARGV.count}): #{ARGV.join(", ")}"
  end

  searchoptions = SearchOptions.new

  settings = nil
  begin
    settings = searchoptions.search_settings_from_args(ARGV)
  rescue ArgumentError => e
    puts "\nArgumentError: #{e.message}\n\n"
    searchoptions.usage
  end

  settings.debug = settings.debug || DEBUG

  if settings.debug
    puts "settings:\n#{settings.to_s}"
  end

  if settings.printusage
    searchoptions.usage
  end

  if settings.printversion
    puts 'Version: 0.1'
    abort
  end

  begin
    searcher = Searcher.new(settings)
    searcher.search

    # print the results
    if settings.printresults
      puts
      searcher.print_results()
    end

    if settings.listdirs
      puts
      dirs = searcher.get_matching_dirs()
      if not dirs.empty?
        puts "Directories with matches (#{dirs.count}):"
        dirs.each do |d|
          puts "#{d}\n"
        end
      end
    end

    if settings.listfiles
      puts
      files = searcher.get_matching_files()
      if not files.empty?
        puts "Files with matches (#{files.count}):"
        files.each do |f|
          puts "#{f}\n"
        end
      end
    end

    if settings.listlines
      puts
      lines = searcher.get_matching_lines()
      if not lines.empty?
        puts "Lines with matches (#{lines.count}):"
        lines.each do |line|
          puts "#{line}\n"
        end
      end
    end

  rescue RuntimeError => e
    puts "\nRuntimeError: #{e.message}\n\n"
    searchoptions.usage
  end

end

if __FILE__ == $0
  main
end
