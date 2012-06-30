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

  if DEBUG
    settings.debug = true
    puts "settings:\n#{settings.to_s}"
  end

  if settings.printusage
    searchoptions.usage
  end

  if settings.printversion
    puts 'Version: 0.1'
    abort
  end

  searcher = Searcher.new(settings)
  searcher.search

end

if __FILE__ == $0
  main
end
