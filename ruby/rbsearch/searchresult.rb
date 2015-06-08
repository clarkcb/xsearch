################################################################################
#
# searchresult.rb
#
# class SearchResult: encapsulates a search result
#
################################################################################

class SearchResult

  attr_accessor :pattern
  attr_accessor :filename
  attr_accessor :linenum
  attr_accessor :match_start_index
  attr_accessor :match_end_index
  attr_accessor :line
  attr_accessor :lines_before
  attr_accessor :lines_after

  @@SEPARATOR_LEN = 80

  def initialize(pattern, filename, linenum=0, match_start_index=0,
     match_end_index=0, line='', lines_before=[], lines_after=[])
    @pattern = pattern
    @filename = filename
    @linenum = linenum
    @match_start_index = match_start_index
    @match_end_index = match_end_index
    @line = line
    @lines_before = lines_before
    @lines_after = lines_after
    @maxlinelength = 150
  end

  def to_s
    if @lines_before.length > 0 or @lines_after.length > 0
      __multiline_s
    else
      __singleline_s
    end
  end

  def __singleline_s
    s = "#{@filename}"
    if @linenum > 0 and not @line.empty?
      s += ": #{@linenum}: [#{@match_start_index}:#{@match_end_index}]:"
      s += " #{__format_matching_line}"
    else
      s += " matches at [#{@match_start_index}:#{@match_end_index}]"
    end
    s
  end

  def __format_matching_line
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
      before = ""
      if before_index > 3
        before = "..."
        before_index += 3
      end
      after = ""
      if after_index < linelength - 3
        after = "..."
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
    while s.end_with? "\n", "\r"
      s = s[0..-2]
    end
    s
  end

  def __multiline_s
    s = "=" * @@SEPARATOR_LEN + "\n"
    s += "#{@filename}: #{@linenum}: [#{@match_start_index}:#{@match_end_index}]\n"
    s += "-" * @@SEPARATOR_LEN + "\n"
    line_format = " %%%dd | %%s\n" % [linenum_padding]
    current_linenum = @linenum
    if @lines_before.length > 0
      current_linenum -= @lines_before.length
      @lines_before.each {|line_before|
        s += ' ' + line_format % [current_linenum, strip_newlines(line_before)]
        current_linenum += 1
      }
    end
    s += '>' + line_format % [current_linenum, strip_newlines(@line)]
    if @lines_after
      current_linenum += 1
      @lines_after.each {|line_after|
        s += ' ' + line_format % [current_linenum, strip_newlines(line_after)]
        current_linenum += 1
      }
    end
    s += "\n"
    s
  end
end
