# frozen_string_literal: true

require_relative 'color'

# SearchResultFormatter - provides formatting of search result instances
class SearchResultFormatter
  attr_accessor :settings

  SEPARATOR_LEN = 80

  def initialize(settings)
    @settings = settings
  end

  def format(result)
    if result.lines_before.empty? && result.lines_after.empty?
      singleline_format(result)
    else
      multiline_format(result)
    end
  end

  private

  def strip_newlines(s)
    s = s[0..-2] while s.end_with? "\n", "\r"
    s
  end

  def colorize(str, match_start_index, match_end_index)
    str[0..(match_start_index - 1)] +
      str[match_start_index..(match_end_index - 1)].green +
      str[match_end_index..(str.length - 1)]
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

    if formatted_length > @settings.maxlinelength
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
      while formatted_length < @settings.maxlinelength
        if line_start_index.positive?
          line_start_index -= 1
          match_start_index += 1
          match_end_index += 1
          formatted_length = line_end_index - line_start_index
        end
        if formatted_length < @settings.maxlinelength && line_end_index < max_line_end_index
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

  def singleline_format(result)
    s = result.file.to_s
    if result.linenum.positive? && !result.line.empty?
      s << ": #{result.linenum}: [#{result.match_start_index}:#{result.match_end_index}]"
      s << ": #{format_matching_line(result)}"
    else
      s << " matches at [#{result.match_start_index}:#{result.match_end_index}]"
    end
    s
  end

  def linenum_padding(result)
    max_linenum = result.linenum + result.lines_after.length
    max_linenum.to_s.length
  end

  def multiline_format(result)
    s = '=' * SEPARATOR_LEN + "\n"
    s << "#{result.file}: #{result.linenum}: [#{result.match_start_index}:#{result.match_end_index}]\n"
    s << '-' * SEPARATOR_LEN + "\n"
    line_format = " %%%dd | %%s\n" % [linenum_padding(result)]
    current_linenum = result.linenum
    unless result.lines_before.empty?
      current_linenum -= result.lines_before.length
      result.lines_before.each do |line_before|
        s << ' ' + line_format % [current_linenum, strip_newlines(line_before)]
        current_linenum += 1
      end
    end
    line = strip_newlines(result.line)
    if @settings.colorize
      line = colorize(line, result.match_start_index - 1, result.match_end_index - 1)
    end
    s << '>' + line_format % [current_linenum, line]
    unless result.lines_after.empty?
      current_linenum += 1
      result.lines_after.each do |line_after|
        s << ' ' + line_format % [current_linenum, strip_newlines(line_after)]
        current_linenum += 1
      end
    end
    s + "\n"
  end
end
