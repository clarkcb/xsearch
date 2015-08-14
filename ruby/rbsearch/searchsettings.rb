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
  attr_accessor :excludehidden
  attr_accessor :firstmatch
  attr_accessor :in_archiveextensions
  attr_accessor :in_archivefilepatterns
  attr_accessor :in_dirpatterns
  attr_accessor :in_extensions
  attr_accessor :in_filepatterns
  attr_accessor :in_linesafterpatterns
  attr_accessor :in_linesbeforepatterns
  attr_accessor :linesafter
  attr_accessor :linesaftertopatterns
  attr_accessor :linesafteruntilpatterns
  attr_accessor :linesbefore
  attr_accessor :listdirs
  attr_accessor :listfiles
  attr_accessor :listlines
  attr_accessor :maxlinelength
  attr_accessor :multilinesearch
  attr_accessor :out_archiveextensions
  attr_accessor :out_archivefilepatterns
  attr_accessor :out_dirpatterns
  attr_accessor :out_extensions
  attr_accessor :out_filepatterns
  attr_accessor :out_linesafterpatterns
  attr_accessor :out_linesbeforepatterns
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
    @archivesonly = false
    @debug = false
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

    @in_archiveextensions = []
    @in_archivefilepatterns = []
    @in_dirpatterns = []
    @in_extensions = []
    @in_filepatterns = []
    @in_linesafterpatterns = []
    @in_linesbeforepatterns = []
    @linesaftertopatterns = []
    @linesafteruntilpatterns = []
    @out_archiveextensions = []
    @out_archivefilepatterns = []
    @out_dirpatterns = []
    @out_extensions = []
    @out_filepatterns = []
    @out_linesafterpatterns = []
    @out_linesbeforepatterns = []
    @searchpatterns = []
  end

  def add_comma_delimited_exts(exts, ext_set)
    exts.split(",").each do |x|
        ext_set.push(x)
    end
  end

  def add_pattern(pattern, pattern_set)
    pattern_set.push(Regexp.new(pattern))
  end

  def list_to_s(name, lst)
    "#{name}=[\"#{lst.join("\", \"")}\"]"
  end

  def to_s
    s = 'SearchSettings('
    s += "archivesonly: #{@archivesonly}"
    s += ", debug: #{@debug}"
    s += ", firstmatch: #{@firstmatch}"
    s += ", excludehidden: #{@excludehidden}"
    s += ", " + list_to_s("in_archiveextensions", @in_archiveextensions)
    s += ", " + list_to_s("in_archivefilepatterns", @in_archivefilepatterns)
    s += ", " + list_to_s("in_dirpatterns", @in_dirpatterns)
    s += ", " + list_to_s("in_extensions", @in_extensions)
    s += ", " + list_to_s("in_filepatterns", @in_filepatterns)
    s += ", " + list_to_s("in_linesafterpatterns", @in_linesafterpatterns)
    s += ", " + list_to_s("in_linesbeforepatterns", @in_linesbeforepatterns)
    s += ", linesafter: #{@linesafter}"
    s += ", " + list_to_s("linesaftertopatterns", @linesaftertopatterns)
    s += ", " + list_to_s("linesafteruntilpatterns", @linesafteruntilpatterns)
    s += ", linesbefore: #{@linesbefore}"
    s += ", listdirs: #{@listdirs}"
    s += ", listfiles: #{@listfiles}"
    s += ", listlines: #{@listlines}"
    s += ", maxlinelength: #{@maxlinelength}"
    s += ", multilinesearch: #{@multilinesearch}"
    s += ", " + list_to_s("out_archiveextensions", @out_archiveextensions)
    s += ", " + list_to_s("out_archivefilepatterns", @out_archivefilepatterns)
    s += ", " + list_to_s("out_dirpatterns", @out_dirpatterns)
    s += ", " + list_to_s("out_extensions", @out_extensions)
    s += ", " + list_to_s("out_filepatterns", @out_filepatterns)
    s += ", " + list_to_s("out_linesafterpatterns", @out_linesafterpatterns)
    s += ", " + list_to_s("out_linesbeforepatterns", @out_linesbeforepatterns)
    s += ", printresults: #{@printresults}"
    s += ", printusage: #{@printusage}"
    s += ", printversion: #{@printversion}"
    s += ", recursive: #{@recursive}"
    s += ", searcharchives: #{@searcharchives}"
    s += ", " + list_to_s("searchpatterns", @searchpatterns)
    s += ", startpath: \"#{@startpath}\""
    s += ", uniquelines: #{@uniquelines}"
    s += ", verbose: #{@verbose}"
    s += ")"
    s
  end
end
