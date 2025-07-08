# frozen_string_literal: true

require 'find'
require 'pathname'
require 'set'

require 'rbfind/common'
require 'rbfind/filetypes'
require 'rbfind/fileutil'
require 'rbfind/fileresult'
require 'rbfind/finder'
require_relative 'searcherror'
require_relative 'searchresult'
require_relative 'searchresultformatter'
require_relative 'searchresultsorter'

module RbSearch

  # Searcher - finds files to search and searches them according to settings
  class Searcher
    attr_reader :settings

    def initialize(settings)
      @settings = settings
      @finder =
        begin
          RbFind::Finder.new(settings)
        rescue RbFind::FindError => e
          raise SearchError, e
        end
      validate_settings
    end

    def search_multi_line_string(str)
      lines_before = []
      lines_after = []
      results = []
      new_line_indices = get_new_line_indices(str)
      start_line_indices = [0] + new_line_indices.map { |n| n + 1 }
      end_line_indices = new_line_indices + [str.length - 1]
      @settings.search_patterns.each do |p|
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
          if @settings.lines_before && before_line_count
            before_start_indices = before_start_indices[0, before_start_indices.size - 1].last(@settings.lines_before)
            lines_before = get_lines_before(str, before_start_indices,
                                            start_line_indices, end_line_indices)
          end
          if @settings.lines_after && after_start_indices
            after_start_indices = after_start_indices[0, @settings.lines_after]
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
            stop = true if @settings.first_match
          end
          m = p.match(str, m_line_start_index + match_end_index)
        end
      end
      results
    end

    def search_line_iterator(lines)
      line_num = 0
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
        line_num += 1
        if @settings.lines_after
          while lines_after.length < @settings.lines_after
            begin
              lines_after.push(lines.next)
            rescue StopIteration
              break
            end
          end
        end
        @settings.search_patterns.each do |p|
          search_line = true
          if @settings.first_match && pattern_matches.include?(p)
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
                    line_num,
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
        if @settings.lines_before
          lines_before.shift if lines_before.size == @settings.lines_before
          lines_before.push(line) if lines_before.size < @settings.lines_before
        end
      end
    end

    def search
      # get the file results
      file_results = @finder.find
      if @settings.verbose
        dir_results = file_results.map(&:path).uniq.sort
        RbFind.log("\nDirectories to be searched (#{dir_results.size}):")
        dir_results.each do |d|
          RbFind.log(d)
        end
        RbFind.log("\nFiles to be searched (#{file_results.size}):")
        file_results.each do |fr|
          RbFind.log(fr.to_s)
        end
        RbFind.log("\n")
      end
      search_results = []
      file_results.each do |fr|
        search_results.concat(search_file(fr))
      end
      if search_results.size > 1
        search_result_sorter = SearchResultSorter.new(@settings)
        search_result_sorter.sort(search_results)
      end
      search_results
    end

    def print_result(search_result, settings)
      s = ''
      s += "#{search_result.pattern}: " if settings.search_patterns.size > 1
      s += search_result.to_s
      RbFind.log(s)
    end

    def print_search_results(search_results, formatter)
      RbFind.log("Search results (#{search_results.size}):")
      search_results.each do |r|
        RbFind.log(formatter.format(r))
      end
    end

    def print_matching_dirs(search_results, formatter)
      file_results = get_file_results(search_results)
      @finder.print_dir_results(file_results, formatter.file_formatter)
    end

    def print_matching_files(search_results, formatter)
      file_results = get_file_results(search_results)
      @finder.print_file_results(file_results, formatter.file_formatter)
    end

    def print_matching_lines(search_results, formatter)
      lines = get_matching_lines(search_results, formatter.settings)
      hdr_text =
        if settings.unique_lines
          'Unique lines with matches'
        else
          'Lines with matches'
        end
      if lines.empty?
        RbFind.log("#{hdr_text}: 0")
      else
        RbFind.log("#{hdr_text} (#{lines.size}):")
        lines.each do |line|
          RbFind.log("#{formatter.format_line(line)}\n")
        end
      end
    end

    private

    def validate_settings
      raise SearchError, 'No search patterns defined' if @settings.search_patterns.empty?
      raise SearchError, 'Invalid lines_before' if @settings.lines_before < 0
      raise SearchError, 'Invalid lines_after' if @settings.lines_after < 0
      raise SearchError, 'Invalid max_line_length' if @settings.max_line_length < 0
      begin
        _enc = Encoding.find(@settings.text_file_encoding)
      rescue StandardError
        raise SearchError, "Invalid encoding: #{@settings.text_file_encoding}"
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

    def search_file(fr)
      case fr.file_type
      when RbFind::FileType::CODE, RbFind::FileType::TEXT, RbFind::FileType::XML
        return search_text_file(fr)
      when RbFind::FileType::BINARY
        return search_binary_file(fr)
      else
        RbFind.log("Searching currently unsupported for FileType #{fr.file_type}")
      end
      []
    end

    def search_binary_file(fr)
      f = File.open(fr.relative_path, 'rb')
      contents = f.read
      results = search_binary_string(contents)
      results.each do |r|
        r.file = fr
        # add_search_result(r)
      end
      return results
    rescue StandardError => e
      raise SearchError, e
    ensure
      f&.close
    end

    def search_binary_string(binstr)
      results = []
      @settings.search_patterns.each do |p|
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
          stop = true if @settings.first_match
        end
      end
      results
    end

    def search_text_file(fr)
      RbFind.log("Searching text file #{fr}") if @settings.debug
      if @settings.multi_line_search
        search_text_file_contents(fr)
      else
        search_text_file_lines(fr)
      end
    end

    def get_line_count(s)
      s.scan(/(\r\n|\n)/m).size
    end

    def search_text_file_contents(fr)
      f = File.open(fr.relative_path, mode: "r:#{@settings.text_file_encoding}")
      contents = f.read
      results = search_multi_line_string(contents)
      results.each do |r|
        r.file = fr
        # add_search_result(r)
      end
      return results
    rescue StandardError => e
      raise SearchError, "#{e} (file: #{fr})"
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
      lines_match(lines_before, @settings.in_lines_before_patterns,
                  @settings.out_lines_before_patterns)
    end

    def do_lines_after_or_until
      !@settings.lines_after_to_patterns.empty? ||
        !@settings.lines_after_until_patterns.empty?
    end

    def do_lines_after
      @settings.lines_after || do_lines_after_or_until
    end

    def lines_after_match(lines_after)
      return true if do_lines_after_or_until
      lines_match(lines_after, @settings.in_lines_after_patterns,
                  @settings.out_lines_after_patterns)
    end

    def search_text_file_lines(fr)
      f = File.open(fr.relative_path, mode: "r:#{@settings.text_file_encoding}")
      line_iterator = f.each_line
      results = search_line_iterator(line_iterator)
      results.each do |r|
        r.file = fr
        # add_search_result(r)
      end
      return results
    rescue StandardError => e
      raise SearchError, "#{e} (file: #{fr})"
    ensure
      f&.close
    end

    def get_file_results(search_results)
      file_set = Set.new
      file_results = []
      search_results.each do |r|
        unless file_set.include?(r.file.to_s)
          file_results.push(r.file)
          file_set.add(r.file.to_s)
        end
      end
      file_results
    end

    def get_matching_lines(results, settings)
      lines = results.map { |r| r.line.strip }.sort { |l1, l2| l1.upcase <=> l2.upcase }
      lines.uniq! if settings.unique_lines
      lines
    end
  end
end
