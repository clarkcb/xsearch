# frozen_string_literal: true

# SearchResult - encapsulates a search result
class SearchResult
  attr_accessor :pattern
  attr_accessor :file
  attr_accessor :linenum
  attr_accessor :match_start_index
  attr_accessor :match_end_index
  attr_accessor :line
  attr_accessor :lines_before
  attr_accessor :lines_after

  SEPARATOR_LEN = 80

  def initialize(pattern, file, linenum=0, match_start_index=0,
                 match_end_index=0, line='', lines_before=[], lines_after=[])
    @pattern = pattern
    @file = file
    @linenum = linenum
    @match_start_index = match_start_index
    @match_end_index = match_end_index
    @line = line
    @lines_before = lines_before
    @lines_after = lines_after
    @maxlinelength = 150
  end

  def to_s
    if @lines_before.empty? && @lines_after.empty?
      singleline_s
    else
      multiline_s
    end
  end

  private

  def singleline_s
    s = @file.to_s
    if @linenum.positive? && !@line.empty?
      s << ": #{@linenum}: [#{@match_start_index}:#{@match_end_index}]:"
      s << " #{@line.strip}"
    else
      s << " matches at [#{@match_start_index}:#{@match_end_index}]"
    end
    s
  end

  def format_matching_line
    formatted = @line
    linelength = @line.length
    matchlength = @match_end_index - @match_start_index
    if linelength > @maxlinelength
      adjusted_maxlength = @maxlinelength - matchlength
      before_index = @match_start_index
      if @match_start_index > 0
        before_index = before_index - (adjusted_maxlength / 4)
        if before_index < 0
          before_index = 0
        end
      end
      adjusted_maxlength = adjusted_maxlength - (@match_start_index - before_index)
      after_index = @match_end_index + adjusted_maxlength
      if after_index > linelength
        after_index = linelength
      end
      before = ''
      if before_index > 3
        before = '...'
        before_index += 3
      end
      after = ''
      if after_index < linelength - 3
        after = '...'
        after_index -= 3
      end
      formatted = before + @line[before_index..after_index] + after
    end
    formatted.strip
  end

  def linenum_padding
    max_linenum = @linenum + @lines_after.length
    max_linenum.to_s.length
  end

  def strip_newlines(s)
    s = s[0..-2] while s.end_with? "\n", "\r"
    s
  end

  def multiline_s
    s = '=' * SEPARATOR_LEN + "\n"
    s << "#{@file}: #{@linenum}: [#{@match_start_index}:#{@match_end_index}]\n"
    s << '-' * SEPARATOR_LEN + "\n"
    line_format = " %%%dd | %%s\n" % [linenum_padding]
    current_linenum = @linenum
    unless @lines_before.empty?
      current_linenum -= @lines_before.length
      @lines_before.each do |line_before|
        s << ' ' + line_format % [current_linenum, strip_newlines(line_before)]
        current_linenum += 1
      end
    end
    s << '>' + line_format % [current_linenum, strip_newlines(@line)]
    unless @lines_after.empty?
      current_linenum += 1
      @lines_after.each do |line_after|
        s << ' ' + line_format % [current_linenum, strip_newlines(line_after)]
        current_linenum += 1
      end
    end
    s + "\n"
  end
end
