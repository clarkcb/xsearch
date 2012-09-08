################################################################################
#
# searcher.rb
#
# class Searcher: executes a file search
#
################################################################################

require 'find'
require 'pathname'
require 'fileutil.rb'
require 'searchresult.rb'

class Searcher
  attr_accessor :results
  attr_accessor :settings

  def initialize(settings)
    @settings = settings
    @fileutil = FileUtil.new
    @results = []
    @timers = {}
    @file_filter_predicates = get_file_filter_predicates
  end

  def get_file_filter_predicates
    file_filter_predicates = []
    if @settings.in_extensions.count > 0
      file_filter_predicates.push(proc { |f|
        @settings.in_extensions.include?(@fileutil.get_extension(f)) })
    end
    if @settings.out_extensions.count > 0
      file_filter_predicates.push(proc { |f|
        not @settings.out_extensions.include?(@fileutil.get_extension(f)) })
    end
    if @settings.in_dirpatterns.count > 0
      file_filter_predicates.push(proc { |f|
        @settings.in_dirpatterns.any? {|p| p.match(File.dirname(f))} })
    end
    if @settings.out_dirpatterns.count > 0
      file_filter_predicates.push(proc { |f|
        not @settings.out_dirpatterns.any? {|p| p.match(File.dirname(f))} })
    end
    if @settings.in_filepatterns.count > 0
      file_filter_predicates.push(proc { |f|
        @settings.in_filepatterns.any? {|p| p.match(Pathname.new(f).basename)} })
    end
    if @settings.out_filepatterns.count > 0
      file_filter_predicates.push(proc { |f|
        not @settings.out_filepatterns.any? {|p| p.match(Pathname.new(f).basename)} })
    end
    file_filter_predicates
  end

  def is_target_file(f)
    @file_filter_predicates.each do |pred|
      if not pred.call(f)
        return false
      end
    end
    true
  end

  def get_search_files
    searchfiles = []
    Find.find(@settings.startpath) do |f|
      if not FileTest.directory?(f)
        searchfiles.push(f) if is_target_file(f)
      end
    end
    searchfiles
  end

  def start_timer(name)
    start = Time.new
    @timers[name+':start'] = start
  end

  def stop_timer(name)
    start = @timers[name+':start']
    stop = Time.new
    @timers[name+':stop'] = stop
    elapsed = stop - start
    @timers[name+':elapsed'] = elapsed
    puts "Elapsed time for #{name}: #{elapsed}"
  end

  def search
    if @settings.dotiming
      start_timer('get_search_files')
    end
    searchfiles = get_search_files()
    if @settings.dotiming
      stop_timer('get_search_files')
    end
    if @settings.verbose
      puts 'Files to be searched:'
      searchfiles.each do |f|
        puts "#{f}"
      end
    end
    if @settings.dotiming
      start_timer('search_files')
    end
    searchfiles.each do |f|
      search_file(f)
    end
    if @settings.dotiming
      stop_timer('search_files')
    end
  end

  def search_file(f)
    if not @fileutil.is_searchable_file(f)
      if @settings.verbose or @settings.debug
        puts "Skipping unsearchable file: #{f}"
        return 0
      end
    end
    matchesfound = 0
    if @fileutil.is_text_file(f)
      matchesfound = search_text_file(f)
    end
    matchesfound
  end

  def search_text_file(f, enc = nil)
    matchesfound = 0
    linenum = 0
    File.open(f, "r").each_line do |line|
      linenum += 1
      @settings.searchpatterns.each do |p|
        if p.match(line)
          add_search_result(SearchResult.new(p, f, linenum, line))
          matchesfound += 1
        end
      end
    end
    matchesfound
  end

  def add_search_result(search_result)
    @results.push(search_result)
    pattern = search_result.pattern
    if @settings.printresults
      print_result(search_result)
    end
  end

  def print_result(search_result)
    s = ""
    if @settings.searchpatterns.count > 1
      s += "#{search_result.pattern}: "
    end
    s += search_result.to_s
    puts s
  end

end
