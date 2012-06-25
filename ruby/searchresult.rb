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

  def initialize(pattern, filename, linenum=0, line='')
    @pattern = pattern
    @filename = filename
    @linenum = linenum
    @line = line
  end

  def to_s
    s = ""
    s += "#{@filename}"
    if @linenum > 0 and not @line.empty?
      s += ": #{@linenum}: #{@line.strip}"
    end
    s
  end
end
