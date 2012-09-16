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
    validate_settings
    @fileutil = FileUtil.new
    @results = []
    @timers = {}
    @file_filter_predicates = get_file_filter_predicates
  end

  def validate_settings
    raise 'Startpath not defined' unless @settings.startpath
    raise 'Startpath not found' unless Pathname.new(@settings.startpath).exist?
    raise 'No search patterns specified' unless @settings.searchpatterns
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
      unless pred.call(f)
        return false
      end
    end
    true
  end

  def get_search_files
    searchfiles = []
    Find.find(@settings.startpath) do |f|
      unless FileTest.directory?(f)
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
    unless @fileutil.is_searchable_file(f)
      if @settings.verbose or @settings.debug
        puts "Skipping unsearchable file: #{f}"
        return 0
      end
    end
    if @fileutil.is_text_file(f)
      search_text_file(f)
    end
  end

  def search_text_file(f, enc = nil)
    linenum = 0
    if @settings.multilinesearch
      search_text_file_contents(f, enc)
    else
      search_text_file_lines(f, enc)
    end
  end

  def get_line_count(s)
    s.scan(/(\r\n|\n)/m).size
  end

  def search_text_file_contents(f, enc = nil)
    contents = File.open(f, "r").read
    @settings.searchpatterns.each do |p|
      m = p.match(contents)
      while m
        before_line_count = get_line_count(m.pre_match)
        after_line_count = get_line_count(m.post_match)
        line_start_index, line_end_index = m.offset(0)
        if before_line_count
          line_start_index = contents.rindex('\n', line_start_index)
        end
        if after_line_count
          line_end_index = contents.index(/(\r\n|\n)/, line_end_index)
        end
        line = contents[line_start_index..line_end_index]
        add_search_result(SearchResult.new(p, f, linenum, line))
        contents = m.post_match
        m = p.match(contents)
      end
    end
  end

  def search_text_file_lines(f, enc = nil)
    linenum = 0
    File.open(f, "r").each_line do |line|
      linenum += 1
      @settings.searchpatterns.each do |p|
        if p.match(line)
          add_search_result(SearchResult.new(p, f, linenum, line))
        end
      end
    end
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
