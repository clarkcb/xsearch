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
  attr_accessor :line

  @@SEPARATOR_LEN = 80

  def initialize(pattern, filename, linenum=0, line='', lines_before=[],
      lines_after=[])
    @pattern = pattern
    @filename = filename
    @linenum = linenum
    @line = line
    @lines_before = lines_before
    @lines_after = lines_after
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
      s += ": #{@linenum}: #{@line.strip}"
    end
    s
  end

  def linenum_padding
    max_linenum = @linenum + @lines_after.length
    max_linenum.to_s.length
  end

  def __multiline_s
    s = "=" * @@SEPARATOR_LEN
    s += "\n#{@filename}\n"
    s += "-" * @@SEPARATOR_LEN
    s += "\n> #{@linenum} | #{@line}\n"
    s
  end
end
