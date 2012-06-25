################################################################################
#
# searchsettings.rb
#
# class SearchSettings: encapsulates search settings
#
################################################################################

class SearchSettings

  attr_accessor :casesensitive
  attr_accessor :debug
  attr_accessor :dotiming
  attr_accessor :firstmatch
  attr_accessor :in_dirpatterns
  attr_accessor :in_extensions
  attr_accessor :in_filepatterns
  attr_accessor :listfiles
  attr_accessor :listlines
  attr_accessor :multilinesearch
  attr_accessor :out_dirpatterns
  attr_accessor :out_extensions
  attr_accessor :out_filepatterns
  attr_accessor :printresults
  attr_accessor :printusage
  attr_accessor :printversion
  attr_accessor :searchcompressed
  attr_accessor :searchpatterns
  attr_accessor :startpath
  attr_accessor :verbose


  def initialize
    @casesensitive = true
    @debug = false
    @dotiming = false
    @firstmatch = false
    @listfiles = false
    @listlines = false
    @multilinesearch = false
    @printresults = true
    @printusage = false
    @printversion = false
    @searchcompressed = false
    @startpath = '.'
    @verbose = false

    @in_dirpatterns = []
    @in_extensions = []
    @in_filepatterns = []
    @out_dirpatterns = []
    @out_extensions = []
    @out_filepatterns = []
    @searchpatterns = []
  end

  def add_searchpattern(pattern, casesensitive=true)
    if @casesensitive
      @searchpatterns.push(Regexp.new(pattern, nil))
    else
      @searchpatterns.push(Regexp.new(pattern, true))
    end
  end

  def to_s
    s = "SearchSettings(startpath: \"#{@startpath}\""
    s += ", searchpatterns: [\"#{@searchpatterns.join("\", \"")}\"]"
    if @in_extensions
      s += ", in_extensions: [#{@in_extensions.join(", ")}]"
    end
    if @out_extensions
      s += ", out_extensions: [#{@out_extensions.join(", ")}]"
    end
    if @in_dirpatterns
      s += ", in_dirpatterns: [#{@in_dirpatterns.join(", ")}]"
    end
    if @out_dirpatterns
      s += ", out_dirpatterns: [#{@out_dirpatterns.join(", ")}]"
    end
    if @in_filepatterns
      s += ", in_filepatterns: [#{@in_filepatterns.join(", ")}]"
    end
    if @out_filepatterns
      s += ", out_filepatterns: [#{@out_filepatterns.join(", ")}]"
    end
    s += ", listfiles: #{@listfiles}" 
    s += ", listlines: #{@listlines}"
    s += ", dotiming: #{@dotiming}"
    s += ", verbose: #{@verbose}"
    s += ", debug: #{@debug}"
    s += ")"
    s
  end

end
