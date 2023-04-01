require_relative 'filetypes'

module RbSearch

  # SearchSettings - encapsulates search settings
  class SearchSettings

    attr_reader :archivesonly
    attr_accessor :colorize
    attr_reader :debug
    attr_accessor :excludehidden
    attr_accessor :firstmatch
    attr_accessor :in_archiveextensions
    attr_accessor :in_archivefilepatterns
    attr_accessor :in_dirpatterns
    attr_accessor :in_extensions
    attr_accessor :in_filepatterns
    attr_accessor :in_filetypes
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
    attr_accessor :out_filetypes
    attr_accessor :out_linesafterpatterns
    attr_accessor :out_linesbeforepatterns
    attr_accessor :paths
    attr_accessor :printresults
    attr_accessor :printusage
    attr_accessor :printversion
    attr_accessor :recursive
    attr_accessor :searcharchives
    attr_accessor :searchpatterns
    attr_accessor :textfileencoding
    attr_accessor :uniquelines
    attr_accessor :verbose

    def initialize
      @archivesonly = false
      @colorize = true
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
      @textfileencoding = 'utf-8'
      @uniquelines = false
      @verbose = false

      @in_archiveextensions = []
      @in_archivefilepatterns = []
      @in_dirpatterns = []
      @in_extensions = []
      @in_filepatterns = []
      @in_filetypes = []
      @in_linesafterpatterns = []
      @in_linesbeforepatterns = []
      @linesaftertopatterns = []
      @linesafteruntilpatterns = []
      @out_archiveextensions = []
      @out_archivefilepatterns = []
      @out_dirpatterns = []
      @out_extensions = []
      @out_filepatterns = []
      @out_filetypes = []
      @out_linesafterpatterns = []
      @out_linesbeforepatterns = []
      @paths = []
      @searchpatterns = []
    end

    def add_exts(exts, ext_set)
      if exts.instance_of? String
        exts.split(',').each do |x|
          ext_set.push(x)
        end
      elsif exts.instance_of? Array
        exts.each do |x|
          ext_set.push(x)
        end
      end
    end

    def add_patterns(patterns, pattern_set)
      if patterns.instance_of? String
        pattern_set.push(Regexp.new(patterns))
      elsif patterns.instance_of? Array
        patterns.each do |p|
          pattern_set.push(Regexp.new(p))
        end
      end
    end

    def add_pattern(pattern, pattern_set)
      pattern_set.push(Regexp.new(pattern))
    end

    def add_filetypes(filetypes, filetypes_set)
      if filetypes.instance_of? String
        filetypes.split(',').each do |t|
          filetypes_set.push(FileTypes.from_name(t))
        end
      elsif filetypes.instance_of? Array
        filetypes.each do |t|
          filetypes_set.push(FileTypes.from_name(t))
        end
      end
    end

    def archivesonly=(bool)
      @archivesonly = bool
      @searcharchives = bool if bool
    end

    def debug=(bool)
      @debug = bool
      @verbose = bool if bool
    end

    def to_s
      s = 'SearchSettings('
      s << "archivesonly: #{@archivesonly}"
      s << ", debug: #{@debug}"
      s << ", firstmatch: #{@firstmatch}"
      s << ", excludehidden: #{@excludehidden}"
      s << ', ' + list_to_s('in_archiveextensions', @in_archiveextensions)
      s << ', ' + list_to_s('in_archivefilepatterns', @in_archivefilepatterns)
      s << ', ' + list_to_s('in_dirpatterns', @in_dirpatterns)
      s << ', ' + list_to_s('in_extensions', @in_extensions)
      s << ', ' + list_to_s('in_filepatterns', @in_filepatterns)
      s << ', ' + filetypes_to_s('in_filetypes', @in_filetypes)
      s << ', ' + list_to_s('in_linesafterpatterns', @in_linesafterpatterns)
      s << ', ' + list_to_s('in_linesbeforepatterns', @in_linesbeforepatterns)
      s << ", linesafter: #{@linesafter}"
      s << ', ' + list_to_s('linesaftertopatterns', @linesaftertopatterns)
      s << ', ' + list_to_s('linesafteruntilpatterns', @linesafteruntilpatterns)
      s << ", linesbefore: #{@linesbefore}"
      s << ", listdirs: #{@listdirs}"
      s << ", listfiles: #{@listfiles}"
      s << ", listlines: #{@listlines}"
      s << ", maxlinelength: #{@maxlinelength}"
      s << ", multilinesearch: #{@multilinesearch}"
      s << ', ' + list_to_s('out_archiveextensions', @out_archiveextensions)
      s << ', ' + list_to_s('out_archivefilepatterns', @out_archivefilepatterns)
      s << ', ' + list_to_s('out_dirpatterns', @out_dirpatterns)
      s << ', ' + list_to_s('out_extensions', @out_extensions)
      s << ', ' + list_to_s('out_filepatterns', @out_filepatterns)
      s << ', ' + filetypes_to_s('out_filetypes', @out_filetypes)
      s << ', ' + list_to_s('out_linesafterpatterns', @out_linesafterpatterns)
      s << ', ' + list_to_s('out_linesbeforepatterns', @out_linesbeforepatterns)
      s << ', ' + list_to_s('paths', @paths)
      s << ", printresults: #{@printresults}"
      s << ", printusage: #{@printusage}"
      s << ", printversion: #{@printversion}"
      s << ", recursive: #{@recursive}"
      s << ", searcharchives: #{@searcharchives}"
      s << ', ' + list_to_s('searchpatterns', @searchpatterns)
      s << ", textfileencoding: \"#{@textfileencoding}\""
      s << ", uniquelines: #{@uniquelines}"
      s << ", verbose: #{@verbose}"
      s << ')'
      s
    end

    private

    def list_to_s(name, lst)
      "#{name}=[\"#{lst.join('", "')}\"]"
    end

    def filetypes_to_s(name, filetypes)
      s = "#{name}=["
      count = 0
      filetypes.each do |ft|
        s << ', ' if count.positive?
        s << "\"#{FileTypes.to_name(ft)}\""
        count += 1
      end
      s + ']'
    end
  end
end
