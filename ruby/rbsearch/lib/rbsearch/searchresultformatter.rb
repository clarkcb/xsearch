# frozen_string_literal: true

require 'rbfind/color'
require 'rbfind/console_color'
require 'rbfind/fileresultformatter'


module RbSearch

  # SearchResultFormatter - provides formatting of search result instances
  class SearchResultFormatter
    attr_reader :settings,
                :file_formatter

    SEPARATOR_LEN = 80

    def initialize(settings)
      @settings = settings
      @file_formatter = RbFind::FileResultFormatter.new(settings)
      if settings.colorize
        define_singleton_method(:format_line) { |line| format_line_with_color(line) }
        define_singleton_method(:format_match) { |match| format_match_with_color(match) }
      end
    end

    def format_line(line)
      line
    end

    def format_match(match)
      match
    end

    def format(result)
      if result.lines_before.empty? && result.lines_after.empty?
        single_line_format(result)
      else
        multi_line_format(result)
      end
    end

    private

    def format_line_with_color(line)
      formatted_line = line
      @settings.search_patterns.each do |p|
        m = p.match(formatted_line)
        if m
          formatted_line = colorize(formatted_line, m.begin(0), m.end(0))
          break
        end
      end
      formatted_line
    end

    def format_match_with_color(match)
      colorize(match, 0, match.length)
    end

    def strip_newlines(s)
      s = s[0..-2] while s.end_with? "\n", "\r"
      s
    end

    def colorize(str, match_start_index, match_end_index)
      @file_formatter.colorize(str, match_start_index, match_end_index, settings.line_color)
    end

    def format_result_match(result)
      if result.line.nil? || result.line.empty?
        return ''
      end
      match_start_idx = result.match_start_index - 1
      match_end_idx = result.match_end_index - 1
      match_length = match_end_idx - match_start_idx

      prefix = ''
      suffix = ''
      color_start_idx = 0
      color_end_idx = match_length

      if match_length > @settings.max_line_length
        if match_start_idx > 2
          prefix = '...'
        end
        suffix = '...'
        color_start_idx = prefix.length
        color_end_idx = @settings.max_line_length - suffix.length
        match_end_idx = match_start_idx + color_end_idx
        match_start_idx = match_start_idx + color_start_idx
      end

      match_string = prefix + result.line[match_start_idx..(match_end_idx - 1)] + suffix
      if @settings.colorize
        match_string = colorize(match_string, color_start_idx, color_end_idx)
      end
      match_string
    end

    def format_result_line(result)
      if result.line.nil? || result.line.strip.empty? || @settings.max_line_length == 0
        return ''
      end

      max_limit = @settings.max_line_length > 0

      if max_limit && (result.match_end_index - result.match_start_index) > @settings.max_line_length
        return format_result_match(result)
      end

      line_start_idx = 0
      line_end_idx = result.line.length - 1

      whitespace_chars = [' ', "\t", "\n", "\r"]
      while whitespace_chars.index(result.line[line_start_idx])
        line_start_idx += 1
      end
      while whitespace_chars.index(result.line[line_end_idx])
        line_end_idx -= 1
      end

      match_length = result.match_end_index - result.match_start_index
      match_start_idx = result.match_start_index - 1 - line_start_idx
      match_end_idx = match_start_idx + match_length

      prefix = ''
      suffix = ''

      trimmed_length = line_end_idx - line_start_idx

      if max_limit && trimmed_length > @settings.max_line_length
        line_start_idx = result.match_start_index - 1
        line_end_idx = line_start_idx + match_length
        match_start_idx = 0
        match_end_idx = match_length

        current_len = line_end_idx - line_start_idx
        while current_len < @settings.max_line_length
          if line_start_idx > 0
            line_start_idx -= 1
            match_start_idx += 1
            match_end_idx += 1
            current_len += 1
          end
          if current_len < @settings.max_line_length && line_end_idx < trimmed_length
            line_end_idx += 1
            current_len += 1
          end
        end

        if line_start_idx > 2
          prefix = '...'
          line_start_idx += 3
        end

        if line_end_idx < (trimmed_length - 3)
          suffix = '...'
          line_end_idx -= 3
        end

      else
        line_end_idx += 1
      end

      formatted = prefix + result.line[line_start_idx..(line_end_idx - 1)] + suffix

      if @settings.colorize
        formatted = colorize(formatted, match_start_idx, match_end_idx)
      end
      formatted
    end

    def single_line_format(result)
      s = @file_formatter.format_file_result(result.file)
      if result.line_num.positive? && !result.line.empty?
        s << ": #{result.line_num}: [#{result.match_start_index}:#{result.match_end_index}]"
        s << ": #{format_result_line(result)}"
      else
        s << " matches at [#{result.match_start_index}:#{result.match_end_index}]"
      end
      s
    end

    def line_num_padding(result)
      max_line_num = result.line_num + result.lines_after.length
      max_line_num.to_s.length
    end

    def multi_line_format(result)
      file = @file_formatter.format_file_result(result.file)
      s = '=' * SEPARATOR_LEN + "\n"
      s << "#{file}: #{result.line_num}: [#{result.match_start_index}:#{result.match_end_index}]\n"
      s << '-' * SEPARATOR_LEN + "\n"
      line_format = " %%%dd | %%s\n" % [line_num_padding(result)]
      current_line_num = result.line_num
      unless result.lines_before.empty?
        current_line_num -= result.lines_before.length
        result.lines_before.each do |line_before|
          s << ' ' + line_format % [current_line_num, strip_newlines(line_before)]
          current_line_num += 1
        end
      end
      line = strip_newlines(result.line)
      if @settings.colorize
        line = colorize(line, result.match_start_index - 1, result.match_end_index - 1)
      end
      s << '>' + line_format % [current_line_num, line]
      unless result.lines_after.empty?
        current_line_num += 1
        result.lines_after.each do |line_after|
          s << ' ' + line_format % [current_line_num, strip_newlines(line_after)]
          current_line_num += 1
        end
      end
      s + "\n"
    end
  end
end
