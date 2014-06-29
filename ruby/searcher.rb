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
  end

  def validate_settings
    raise 'Startpath not defined' unless @settings.startpath
    raise 'Startpath not found' unless Pathname.new(@settings.startpath).exist?
    raise 'No search patterns specified' unless @settings.searchpatterns
  end

  def matches_any_pattern(s, pattern_set)
    pattern_set.any? {|p| p.match(s)}
  end

  def any_matches_any_pattern(slist, pattern_set)
    slist.each do |s|
      if matches_any_pattern(s, pattern_set)
        return true
      end
    end
    return false
  end

  def is_search_dir(d)
    result = true
    path_elems = d.split(File::SEPARATOR)
    if @settings.in_dirpatterns.count > 0 and
      not any_matches_any_pattern(path_elems, @settings.in_dirpatterns)
      return false
    end
    if @settings.out_dirpatterns.count > 0 and
      any_matches_any_pattern(path_elems, @settings.out_dirpatterns)
      return false
    end
    true
  end

  def is_search_file(f)
    if @settings.in_extensions.count > 0 and 
      not @settings.in_extensions.include?(@fileutil.get_extension(f))
      return false
    end
    if @settings.out_extensions.count > 0 and
      @settings.out_extensions.include?(@fileutil.get_extension(f))
      return false
    end
    filename = Pathname.new(f).basename.to_s
    if @settings.in_filepatterns.count > 0 and
      not matches_any_pattern(filename, @settings.in_filepatterns)
      return false
    end
    if @settings.out_filepatterns.count > 0 and
      matches_any_pattern(filename, @settings.out_filepatterns)
      return false
    end
    true
  end

  def get_search_dirs
    searchdirs = []
    Find.find(@settings.startpath) do |f|
      if FileTest.directory?(f)
        searchdirs.push(f) if is_search_dir(f)
      end
    end
    searchdirs
  end

  def get_search_files(searchdirs)
    searchfiles = []
    searchdirs.each do |d|
        all_dirs = Dir.entries(d)
        all_dirs.each do |f|
          unless FileTest.directory?(f)
            searchfiles.push(Pathname.new(d).join(f).to_s) if is_search_file(f)
          end
        end
      end
    searchfiles
  end

  def add_timer(name, action)
    @timers[name+':'+action] = Time.new
  end

  def start_timer(name)
    add_timer(name, 'start')
  end

  def get_elapsed(name)
    start = @timers[name+':start']
    stop = @timers[name+':stop']
    stop - start
  end

  def print_elapsed(name)
    elapsed = get_elapsed(name)
    puts "Elapsed time for #{name}: #{elapsed}"
  end

  def stop_timer(name)
    add_timer(name, 'stop')
    print_elapsed(name)
  end

  def search
    # get the searchdirs
    if @settings.dotiming
      start_timer('get_search_dirs')
    end
    searchdirs = get_search_dirs()
    if @settings.dotiming
      stop_timer('get_search_dirs')
    end
    if @settings.verbose
      puts "\nDirectories to be searched (#{searchdirs.count}):"
      searchdirs.each do |d|
        puts "#{d}"
      end
    end
    # get the searchfiles
    if @settings.dotiming
      start_timer('get_search_files')
    end
    searchfiles = get_search_files(searchdirs)
    if @settings.dotiming
      stop_timer('get_search_files')
    end
    if @settings.verbose
      puts "\nFiles to be searched (#{searchfiles.count}):"
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

    # print the results
    if @settings.printresults
      puts
      print_results()
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
  end

  def print_results()
    puts "Search results (#{@results.count}):"
    @results.each do |r|
      print_result(r)
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
