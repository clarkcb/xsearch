################################################################################
#
# searcher.rb
#
# class Searcher: executes a file search
#
################################################################################

require 'find'
require 'pathname'
require 'set'
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
    @filehash = Hash.new([])
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
    path_elems = d.split(File::SEPARATOR) - ['.', '..']
    if @settings.excludehidden and path_elems.any? {|p| p.start_with?('.')}
      return false
    end
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
    if @settings.excludehidden and f.start_with?('.')
      return false
    end
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

  def is_archive_search_file(f)
    if @settings.excludehidden and f.start_with?('.')
      return false
    end
    if @settings.in_archiveextensions.count > 0 and 
      not @settings.in_archiveextensions.include?(@fileutil.get_extension(f))
      return false
    end
    if @settings.out_archiveextensions.count > 0 and
      @settings.out_archiveextensions.include?(@fileutil.get_extension(f))
      return false
    end
    filename = Pathname.new(f).basename.to_s
    if @settings.in_archivefilepatterns.count > 0 and
      not matches_any_pattern(filename, @settings.in_archivefilepatterns)
      return false
    end
    if @settings.out_archivefilepatterns.count > 0 and
      matches_any_pattern(filename, @settings.out_archivefilepatterns)
      return false
    end
    true
  end

  def get_search_dirs
    searchdirs = []
    if @settings.recursive
      Find.find(@settings.startpath) do |f|
        if FileTest.directory?(f)
          searchdirs.push(f) if is_search_dir(f)
        end
      end
    else
      searchdirs.push(@settings.startpath) if is_search_dir(@settings.startpath)
    end
    searchdirs
  end

  def get_search_files(searchdirs)
    searchfiles = []
    searchdirs.each do |d|
        all_dirs = Dir.entries(d)
        all_dirs.each do |f|
          unless FileTest.directory?(f)
            if @fileutil.is_archive_file(f) and @settings.searcharchives and is_archive_search_file(f)
              searchfiles.push(Pathname.new(d).join(f).to_s)
            elsif not @settings.archivesonly and is_search_file(f)
              searchfiles.push(Pathname.new(d).join(f).to_s)
            end
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
      puts "\n"
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
    elsif @fileutil.is_binary_file(f)
      search_binary_file(f)
    end
  end

  def search_binary_file(f)
    contents = File.open(f, "rb").read
    @settings.searchpatterns.each do |p|
      if p.match(contents)
        add_search_result(SearchResult.new(p, f, 0, nil))
      end
    end
  end

  def search_text_file(f, enc = nil)
    if @settings.debug
      puts "Searching text file #{f}"
    end
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
    results = search_contents(contents)
    results.each do |r|
      r.filename = f
      add_search_result(r)
    end
  end

  def search_contents(contents)
    results = []
    @settings.searchpatterns.each do |p|
      m = p.match(contents)
      while m
        before_line_count = get_line_count(m.pre_match)
        after_line_count = get_line_count(m.post_match)
        line_start_index, line_end_index = m.offset(0)
        if before_line_count > 0
          line_start_index = contents.rindex("\n", line_start_index) + 1
        end
        if after_line_count > 0
          line_end_index = contents.index(/(\r\n|\n)/, line_end_index) - 1
        end
        line = contents[line_start_index..line_end_index]
        match_start_index = m.begin(0) - line_start_index
        match_end_index = m.end(0) - line_start_index
        results.push(SearchResult.new(p, '', before_line_count+1, line,
          match_start_index, match_end_index))
        m = p.match(contents, line_start_index+match_end_index)
      end
    end
    results
  end

  def search_text_file_lines(f, enc = nil)
    linenum = 0
    fo = File.open(f, "r")
    line_iterator = fo.each_line
    results = search_line_iterator(line_iterator)
    fo.close
    results.each do |r|
      r.filename = f
      add_search_result(r)
    end
  end

  def search_line_iterator(lines)
    linenum = 0
    pattern_matches = {}
    results = []
    while true
      begin
        line = lines.next
        linenum += 1
        @settings.searchpatterns.each do |p|
          search_line = true
          pos = 0
          while search_line and pos < line.length
            # TODO: catch ArgumentError: "in `match': invalid byte sequence in US-ASCII"
            m = p.match(line, pos)
            if m
              if @settings.firstmatch and pattern_matches.include?(p)
                search_line = false
              else
                results.push(SearchResult.new(p, '', linenum, line, m.begin(0),
                  m.end(0)))
                pos = m.end(0) + 1
                pattern_matches[p] = 1
              end
            else
              search_line = false
            end
          end
        end
      rescue StopIteration
        return results
      end
    end
  end

  def add_search_result(search_result)
    @results.push(search_result)
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

  def get_matching_dirs(patterns = [])
    if patterns.empty?
      patterns = @settings.searchpatterns
    end
    pattern_set = Set.new patterns
    dirs = Set.new
    @results.each do |r|
      if pattern_set.include? r.pattern
        dirs.add(File.dirname(r.filename))
      end
    end
    dirs = dirs.to_a
    dirs.sort!
    dirs
  end

  def get_matching_files(patterns = [])
    if patterns.empty?
      patterns = @settings.searchpatterns
    end
    pattern_set = Set.new patterns
    files = Set.new
    @results.each do |r|
      if pattern_set.include? r.pattern
        files.add(r.filename)
      end
    end
    files = files.to_a
    files.sort!
    files
  end

  def get_matching_lines(patterns = [])
    if patterns.empty?
      patterns = @settings.searchpatterns
    end
    pattern_set = Set.new patterns
    lines = []
    @results.each do |r|
      if pattern_set.include? r.pattern
        lines.push(r.line.strip)
      end
    end
    lines.sort!
    lines
  end
end
