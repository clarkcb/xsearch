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
  rescue RuntimeError => e
    puts "\nRuntimeError: #{e.message}\n\n"
    searchoptions.usage
  end

end

if __FILE__ == $0
  main
end
