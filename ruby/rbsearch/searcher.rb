# frozen_string_literal: true

require 'find'
require 'pathname'
require 'set'
require_relative 'common.rb'
require_relative 'filetypes.rb'
require_relative 'fileutil.rb'
require_relative 'searcherror.rb'
require_relative 'searchfile.rb'
require_relative 'searchresult.rb'
require_relative 'searchresultformatter.rb'

# Searcher - finds files to search and searches them according to settings
class Searcher
  attr_reader :settings
  attr_reader :results

  def initialize(settings)
    @settings = settings
    validate_settings
    @filetypes = FileTypes.new
    @results = []
  end

  def search_dir?(dirname)
    path_elems = dirname.split(File::SEPARATOR) - FileUtil.dot_dirs
    if @settings.excludehidden && path_elems.any? { |p| FileUtil.hidden?(p) }
      return false
    end
    if !@settings.in_dirpatterns.empty? &&
       !any_matches_any_pattern(path_elems, @settings.in_dirpatterns)
      return false
    end
    if !@settings.out_dirpatterns.empty? &&
       any_matches_any_pattern(path_elems, @settings.out_dirpatterns)
      return false
    end
    true
  end

  def search_file?(filename)
    ext = FileUtil.get_extension(filename)
    if !@settings.in_extensions.empty? &&
       !@settings.in_extensions.include?(ext)
      return false
    end
    if !@settings.out_extensions.empty? &&
       @settings.out_extensions.include?(ext)
      return false
    end
    if !@settings.in_filepatterns.empty? &&
       !matches_any_pattern(filename, @settings.in_filepatterns)
      return false
    end
    if !@settings.out_filepatterns.empty? &&
       matches_any_pattern(filename, @settings.out_filepatterns)
      return false
    end
    filetype = @filetypes.get_filetype(filename)
    if !@settings.in_filetypes.empty? &&
       !@settings.in_filetypes.include?(filetype)
      return false
    end
    if !@settings.out_filetypes.empty? &&
       @settings.out_filetypes.include?(filetype)
      return false
    end
    true
  end

  def archive_search_file?(filename)
    ext = FileUtil.get_extension(filename)
    if !@settings.in_archiveextensions.empty? &&
       !@settings.in_archiveextensions.include?(ext)
      return false
    end
    if !@settings.out_archiveextensions.empty? &&
       @settings.out_archiveextensions.include?(ext)
      return false
    end
    if !@settings.in_archivefilepatterns.empty? &&
       !matches_any_pattern(filename, @settings.in_archivefilepatterns)
      return false
    end
    if !@settings.out_archivefilepatterns.empty? &&
       matches_any_pattern(filename, @settings.out_archivefilepatterns)
      return false
    end
    true
  end

  def filter_file?(filename)
    if @settings.excludehidden && FileUtil.hidden?(filename)
      return false
    end
    if @filetypes.archive_file?(filename)
      return @settings.searcharchives && archive_search_file?(filename)
    end
    !@settings.archivesonly && search_file?(filename)
  end

  def search
    # get the searchfiles
    searchfiles = get_search_files.sort_by(&:relativepath)
    if @settings.verbose
      searchdirs = searchfiles.map(&:path).uniq.sort
      log("\nDirectories to be searched (#{searchdirs.size}):")
      searchdirs.each do |d|
        log(d)
      end
      log("\nFiles to be searched (#{searchfiles.size}):")
      searchfiles.each do |sf|
        log(sf.to_s)
      end
      log("\n")
    end
    searchfiles.each do |sf|
      search_file(sf)
    end
  end

  def search_multiline_string(str)
    lines_before = []
    lines_after = []
    results = []
    new_line_indices = get_new_line_indices(str)
    start_line_indices = [0] + new_line_indices.map { |n| n + 1 }
    end_line_indices = new_line_indices + [str.length - 1]
    @settings.searchpatterns.each do |p|
      m = p.match(str)
      stop = false
      while m && !stop
        m_line_start_index = 0
        m_line_end_index = str.length - 1
        before_start_indices = start_line_indices.take_while { |i| i <= m.begin(0) }
        before_line_count = 0
        if before_start_indices
          m_line_start_index = before_start_indices.last
          before_line_count = before_start_indices.size - 1
        end
        m_line_end_index = end_line_indices[start_line_indices.index(m_line_start_index)]
        after_start_indices = start_line_indices.drop_while { |i| i <= m.begin(0) }
        line = str[m_line_start_index..m_line_end_index]
        if @settings.linesbefore && before_line_count
          before_start_indices = before_start_indices[0, before_start_indices.size - 1].last(@settings.linesbefore)
          lines_before = get_lines_before(str, before_start_indices,
                                          start_line_indices, end_line_indices)
        end
        if @settings.linesafter && after_start_indices
          after_start_indices = after_start_indices[0, @settings.linesafter]
          lines_after = get_lines_after(str, after_start_indices,
                                        start_line_indices, end_line_indices)
        end
        match_start_index = m.begin(0) - m_line_start_index
        match_end_index = m.end(0) - m_line_start_index
        if (lines_before.length == 0 || lines_before_match(lines_before)) &&
           (lines_after.length == 0 || lines_after_match(lines_after))
          results.push(SearchResult.new(
            p,
            nil,
            before_line_count + 1,
            match_start_index + 1,
            match_end_index + 1,
            line,
            lines_before,
            lines_after
          ))
          stop = true if @settings.firstmatch
        end
        m = p.match(str, m_line_start_index + match_end_index)
      end
    end
    results
  end

  def search_line_iterator(lines)
    linenum = 0
    lines_before = []
    lines_after = []
    pattern_matches = {}
    results = []
    loop do
      line =
        if lines_after.empty?
          begin
            lines.next
          rescue StopIteration
            return results
          end
        else
          lines_after.shift
        end
      linenum += 1
      if @settings.linesafter
        while lines_after.length < @settings.linesafter
          begin
            lines_after.push(lines.next)
          rescue StopIteration
            break
          end
        end
      end
      @settings.searchpatterns.each do |p|
        search_line = true
        if @settings.firstmatch && pattern_matches.include?(p)
          search_line = false
        end
        pos = 0
        while search_line && pos < line.length
          begin
            m = p.match(line, pos)
            if m
              if (!lines_before.empty? && !lines_before_match(lines_before)) ||
                 (!lines_after.empty? && !lines_after_match(lines_after))
                search_line = false
              else
                results.push(SearchResult.new(
                  p,
                  nil,
                  linenum,
                  m.begin(0) + 1,
                  m.end(0) + 1,
                  line,
                  [].concat(lines_before),
                  [].concat(lines_after)
                ))
                pos = m.end(0) + 1
                pattern_matches[p] = 1
              end
            else
              search_line = false
            end
          rescue StandardError => e
            raise SearchError, e
          end
        end
      end
      if @settings.linesbefore
        lines_before.shift if lines_before.size == @settings.linesbefore
        lines_before.push(line) if lines_before.size < @settings.linesbefore
      end
    end
  end

  def print_results
    formatter = SearchResultFormatter.new(@settings)
    log("Search results (#{@results.size}):")
    @results.each do |r|
      # print_result(r)
      log(formatter.format(r))
    end
  end

  def print_result(search_result)
    s = ''
    s += "#{search_result.pattern}: " if @settings.searchpatterns.size > 1
    s += search_result.to_s
    log(s)
  end

  def get_matching_dirs
    @results.map { |r| r.file.path }.uniq.sort
  end

  def get_matching_files
    @results.map { |r| r.file.to_s }.uniq.sort
  end

  def get_matching_lines
    lines = @results.map { |r| r.line.strip }.sort { |l1, l2| l1.upcase <=> l2.upcase }
    lines.uniq! if @settings.uniquelines
    lines
  end

  private

  def validate_settings
    raise SearchError, 'Startpath not defined' unless @settings.startpath
    raise SearchError, 'Startpath not found' unless Pathname.new(@settings.startpath).exist?
    raise SearchError, 'Startpath not readable' unless File.readable?(@settings.startpath)
    raise SearchError, 'No search patterns defined' if @settings.searchpatterns.empty?
    raise SearchError, 'Invalid linesbefore' if @settings.linesbefore < 0
    raise SearchError, 'Invalid linesafter' if @settings.linesafter < 0
    raise SearchError, 'Invalid maxlinelength' if @settings.maxlinelength < 0
    begin
      _enc = Encoding.find(@settings.textfileencoding)
    rescue StandardError
      raise SearchError, "Invalid encoding: #{@settings.textfileencoding}"
    end
  end

  def matches_any_pattern(str, pattern_set)
    pattern_set.any? { |p| p.match(str) }
  end

  def any_matches_any_pattern(str_list, pattern_set)
    str_list.each do |s|
      return true if matches_any_pattern(s, pattern_set)
    end
    false
  end

  def file_to_searchfile(f)
    d = File.dirname(f) || '.'
    filename = File.basename(f)
    filetype = @filetypes.get_filetype(filename)
    SearchFile.new(d, filename, filetype)
  end

  def get_search_files
    searchfiles = []
    if FileTest.directory?(@settings.startpath)
      if @settings.recursive
        Find.find(@settings.startpath) do |f|
          if FileTest.directory?(f)
            Find.prune unless search_dir?(f)
          elsif filter_file?(f)
            searchfile = file_to_searchfile(f)
            searchfiles.push(searchfile)
          end
        end
      else
        Find.find(@settings.startpath) do |f|
          if FileTest.directory?(f)
            Find.prune
          elsif filter_file?(f)
            searchfile = file_to_searchfile(f)
            searchfiles.push(searchfile)
          end
        end
      end
    elsif FileTest.file?(@settings.startpath)
      searchfile = file_to_searchfile(@settings.startpath)
      searchfiles.push(searchfile)
    end
    searchfiles
  end

  def search_file(sf)
    unless @filetypes.searchable_file?(sf.filename)
      if @settings.verbose || @settings.debug
        log("Skipping unsearchable file: #{sf}")
        return 0
      end
    end
    case sf.filetype
    when FileType::CODE, FileType::TEXT, FileType::XML
      search_text_file(sf)
    when FileType::BINARY
      search_binary_file(sf)
    else
      log("Searching currently unsupported for FileType #{sf.filetype}")
    end
  end

  def search_binary_file(sf)
    f = File.open(sf.relativepath, 'rb')
    contents = f.read
    results = search_binary_string(contents)
    results.each do |r|
      r.file = sf
      add_search_result(r)
    end
  rescue StandardError => e
    raise SearchError, e
  ensure
    f&.close
  end

  def search_binary_string(binstr)
    results = []
    @settings.searchpatterns.each do |p|
      pos = 0
      m = p.match(binstr, pos)
      stop = false
      while m && !stop
        results.push(SearchResult.new(
          p,
          nil,
          0,
          m.begin(0) + 1,
          m.end(0) + 1,
          nil
        ))
        pos = m.end(0) + 1
        m = p.match(binstr, pos)
        stop = true if @settings.firstmatch
      end
    end
    results
  end

  def search_text_file(sf)
    log("Searching text file #{sf}") if @settings.debug
    if @settings.multilinesearch
      search_text_file_contents(sf)
    else
      search_text_file_lines(sf)
    end
  end

  def get_line_count(s)
    s.scan(/(\r\n|\n)/m).size
  end

  def search_text_file_contents(sf)
    f = File.open(sf.relativepath, mode: "r:#{@settings.textfileencoding}")
    contents = f.read
    results = search_multiline_string(contents)
    results.each do |r|
      r.file = sf
      add_search_result(r)
    end
  rescue StandardError => e
    raise SearchError, "#{e} (file: #{sf})"
  ensure
    f&.close
  end

  def get_new_line_indices(s)
    indices = []
    i = 0
    s.chars.each do |c|
      indices.push(i) if c == "\n"
      i += 1
    end
    indices
  end

  def get_lines_at_indices(s, at_indices, start_line_indices, end_line_indices)
    lines = []
    at_indices.each do |i|
      line = s[i..end_line_indices[start_line_indices.index(i)]]
      lines.push(line)
    end
    lines
  end

  def get_lines_before(s, before_start_indices, start_line_indices, end_line_indices)
    get_lines_at_indices(s, before_start_indices, start_line_indices, end_line_indices)
  end

  def get_lines_after(s, after_start_indices, start_line_indices, end_line_indices)
    get_lines_at_indices(s, after_start_indices, start_line_indices, end_line_indices)
  end

  def lines_match(lines, in_patterns, out_patterns)
    ((in_patterns.empty? || any_matches_any_pattern(lines, in_patterns)) &&
    (out_patterns.empty? || !any_matches_any_pattern(lines, out_patterns)))
  end

  def lines_before_match(lines_before)
    lines_match(lines_before, @settings.in_linesbeforepatterns,
      @settings.out_linesbeforepatterns)
  end

  def do_lines_after_or_until
    !@settings.linesaftertopatterns.empty? ||
      !@settings.linesafteruntilpatterns.empty?
  end

  def do_lines_after
    @settings.linesafter || do_lines_after_or_until
  end

  def lines_after_match(lines_after)
    return true if do_lines_after_or_until
    lines_match(lines_after, @settings.in_linesafterpatterns,
      @settings.out_linesafterpatterns)
  end

  def search_text_file_lines(sf)
    f = File.open(sf.relativepath, mode: "r:#{@settings.textfileencoding}")
    line_iterator = f.each_line
    results = search_line_iterator(line_iterator)
    results.each do |r|
      r.file = sf
      add_search_result(r)
    end
  rescue StandardError => e
    raise SearchError, "#{e} (file: #{sf})"
  ensure
    f&.close
  end

  def add_search_result(search_result)
    @results.push(search_result)
  end
end
