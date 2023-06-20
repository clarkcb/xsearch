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

module RbSearch

  # Searcher - finds files to search and searches them according to settings
  class Searcher
    attr_reader :settings

    def initialize(settings)
      @settings = settings
      validate_settings
      @finder = RbFind::Finder.new(settings)
    end

    def search
      # get the file results
      file_results = @finder.find
      if @settings.verbose
        dir_results = file_results.map(&:path).uniq.sort
        RbFind::log("\nDirectories to be searched (#{dir_results.size}):")
        dir_results.each do |d|
          RbFind::log(d)
        end
        RbFind::log("\nFiles to be searched (#{file_results.size}):")
        file_results.each do |fr|
          RbFind::log(fr.to_s)
        end
        RbFind::log("\n")
      end
      results = []
      file_results.each do |fr|
        results.concat(search_file(fr))
      end
      results
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


    private

    def validate_settings
      raise SearchError, 'Startpath not defined' if @settings.paths.empty?
      @settings.paths.each do |p|
        raise SearchError, 'Startpath not found' unless Pathname.new(p).exist?
        raise SearchError, 'Startpath not readable' unless File.readable?(p)
      end
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
        RbFind::log("Searching currently unsupported for FileType #{fr.file_type}")
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
      RbFind::log("Searching text file #{fr}") if @settings.debug
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
  end
end
