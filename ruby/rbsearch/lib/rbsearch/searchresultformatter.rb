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

    def format_matching_line(result)
      formatted = result.line
      leading_ws_count = 0
      whitespace_chars = [' ', "\t", "\n", "\r"]
      while whitespace_chars.index(formatted[leading_ws_count])
        leading_ws_count += 1
      end
      formatted = formatted.strip
      formatted_length = formatted.length
      max_line_end_index = formatted_length - 1
      match_length = result.match_end_index - result.match_start_index

      match_start_index = result.match_start_index - 1 - leading_ws_count
      match_end_index = match_start_index + match_length

      if formatted_length > @settings.max_line_length
        line_start_index = match_start_index
        line_end_index = line_start_index + match_length
        match_start_index = 0
        match_end_index = match_length

        while line_end_index > formatted_length - 1
          line_start_index -= 1
          line_end_index -= 1
          match_start_index += 1
          match_end_index += 1
        end

        formatted_length = line_end_index - line_start_index
        while formatted_length < @settings.max_line_length
          if line_start_index.positive?
            line_start_index -= 1
            match_start_index += 1
            match_end_index += 1
            formatted_length = line_end_index - line_start_index
          end
          if formatted_length < @settings.max_line_length && line_end_index < max_line_end_index
            line_end_index += 1
          end
          formatted_length = line_end_index - line_start_index
        end

        formatted = formatted[line_start_index..(line_end_index - 1)]

        if line_start_index > 2
          formatted = "...#{formatted[3..(formatted.length - 1)]}"
        end
        if line_end_index < max_line_end_index - 3
          formatted = "#{formatted[0..(formatted.length - 4)]}..."
        end
      end

      if @settings.colorize
        formatted = colorize(formatted, match_start_index, match_end_index)
      end
      formatted
    end

    def single_line_format(result)
      s = @file_formatter.format_file_result(result.file)
      if result.line_num.positive? && !result.line.empty?
        s << ": #{result.line_num}: [#{result.match_start_index}:#{result.match_end_index}]"
        s << ": #{format_matching_line(result)}"
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
