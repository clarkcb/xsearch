################################################################################
#
# searchsettings.rb
#
# class SearchSettings: encapsulates search settings
#
################################################################################

class SearchSettings

  attr_accessor :archivesonly
  attr_accessor :debug
  attr_accessor :dotiming
  attr_accessor :excludehidden
  attr_accessor :firstmatch
  attr_accessor :in_archivefilepatterns
  attr_accessor :in_dirpatterns
  attr_accessor :in_extensions
  attr_accessor :in_filepatterns
  attr_accessor :linesafter
  attr_accessor :linesaftertopatterns
  attr_accessor :linesafteruntilpatterns
  attr_accessor :linesbefore
  attr_accessor :listdirs
  attr_accessor :listfiles
  attr_accessor :listlines
  attr_accessor :maxlinelength
  attr_accessor :multilinesearch
  attr_accessor :out_archivefilepatterns
  attr_accessor :out_dirpatterns
  attr_accessor :out_extensions
  attr_accessor :out_filepatterns
  attr_accessor :printresults
  attr_accessor :printusage
  attr_accessor :printversion
  attr_accessor :recursive
  attr_accessor :searcharchives
  attr_accessor :searchpatterns
  attr_accessor :startpath
  attr_accessor :uniquelines
  attr_accessor :verbose

  def initialize
    @archivesonly = true
    @debug = false
    @dotiming = false
    @excludehidden = true
    @firstmatch = false
    @linesafter = 0
    @linesbefore = 0
    @listdirs = false
    @listfiles = false
    @listlines = false
    @maxlinelength = 150
    @multilinesearch = false
    @printresults = true
    @printusage = false
    @printversion = false
    @recursive = true
    @searcharchives = false
    @startpath = nil
    @uniquelines = false
    @verbose = false

    @in_archivefilepatterns = []
    @in_dirpatterns = []
    @in_extensions = []
    @in_filepatterns = []
    @in_linesafterpatterns = []
    @in_linesbeforepatterns = []
    @linesaftertopatterns = []
    @linesafteruntilpatterns = []
    @out_archivefilepatterns = []
    @out_dirpatterns = get_default_out_dirpatterns()
    @out_extensions = []
    @out_filepatterns = get_default_out_filepatterns()
    @out_linesafterpatterns = []
    @out_linesbeforepatterns = []
    @searchpatterns = []
  end

  def get_default_out_dirpatterns()
    out_dirs = ["\\bCVS$", "\\.git$", "\\.svn$"]
    out_dirpatterns = []
    out_dirs.each do |d|
      out_dirpatterns.push(Regexp.new(d))
    end
    out_dirpatterns
  end

  def get_default_out_filepatterns()
    out_files = ["^\\.DS_Store$"]
    out_filepatterns = []
    out_files.each do |f|
      out_filepatterns.push(Regexp.new(f))
    end
    out_filepatterns
  end

  def add_comma_delimited_exts(exts, ext_set)
    exts.split(",").each do |x|
        ext_set.push(x)
    end
  end

  def add_searchpattern(pattern)
    @searchpatterns.push(Regexp.new(pattern, true))
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
    s += ", archivesonly: #{@archivesonly}"
    s += ", debug: #{@debug}"
    s += ", dotiming: #{@dotiming}"
    s += ", firstmatch: #{@firstmatch}"
    s += ", excludehidden: #{@excludehidden}"
    s += ", linesafter: #{@linesafter}"
    s += ", linesbefore: #{@linesbefore}"
    s += ", listdirs: #{@listdirs}"
    s += ", listfiles: #{@listfiles}"
    s += ", listlines: #{@listlines}"
    s += ", maxlinelength: #{@maxlinelength}"
    s += ", multilinesearch: #{@multilinesearch}"
    s += ", printresults: #{@printresults}"
    s += ", printusage: #{@printusage}"
    s += ", printversion: #{@printversion}"
    s += ", recursive: #{@recursive}"
    s += ", searcharchives: #{@searcharchives}"
    s += ", uniquelines: #{@uniquelines}"
    s += ", verbose: #{@verbose}"
    s += ")"
    s
  end
end
