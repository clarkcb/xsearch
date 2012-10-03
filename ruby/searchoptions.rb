################################################################################
#
# searchoptions.rb
#
# class SearchOptions: defines the available command-line options and
#                      corresponding utility methods
#
################################################################################

require 'searchoption.rb'
require 'searchsettings.rb'

class SearchOptions
  
  def initialize
    set_options
    @options = []
    @options.concat(@arg_options)
    @options.concat(@flag_options)
    @options.sort! { |a, b| a.sortarg <=> b.sortarg }
    @arg_dict = dict_from_options(@arg_options)
    @flag_dict = dict_from_options(@flag_options)
  end

  def set_options
    @arg_options = [
      SearchOption.new('b', 'numlinesbefore',
        'Number of lines to show before every matched line (default: 0)',
        proc { |x, settings| puts "numlinesbefore: #{x}" }),
      SearchOption.new('B', 'numlinesafter',
        'Number of lines to show after every matched line (default: 0)',
        proc { |x, settings| puts "numlinesafter: #{x}" }),
      SearchOption.new('d', 'dirname',
        'Specify name pattern for directories to include in search',
        proc { |x, settings| settings.in_dirpatterns.push(Regexp.new(x)) }),
      SearchOption.new('D', 'dirfilter',
        'Specify name pattern for directories to exclude from search',
        proc { |x, settings| settings.out_dirpatterns.push(Regexp.new(x)) }),
      SearchOption.new('f', 'filename',
        'Specify name pattern for files to include in search',
        proc { |x, settings| settings.in_filepatterns.push(Regexp.new(x)) }),
      SearchOption.new('F', 'filefilter',
        'Specify name pattern for files to exclude from search',
        proc { |x, settings| settings.out_filepatterns.push(Regexp.new(x)) }),
      SearchOption.new('', 'linesafterfilter',
        'Specify pattern to filter the "lines-after" lines on (used with --numlinesafter)',
        proc { |x, settings| puts "linesafterfilter: #{x}" }),
      SearchOption.new('', 'linesaftersearch',
        'Specify pattern to search the "lines-after" lines on (used with --numlinesafter)',
        proc { |x, settings| puts "linesaftersearch: #{x}" }),
      SearchOption.new('', 'linesbeforefilter',
        'Specify pattern to filter the "lines-before" lines on (used with --numlinesbefore)',
        proc { |x, settings| puts "linesbeforefilter: #{x}" }),
      SearchOption.new('', 'linesbeforesearch',
        'Specify pattern to search the "lines-before" lines on (used with --numlinesbefore)',
        proc { |x, settings| puts "linesbeforesearch: #{x}" }),
      SearchOption.new('s', 'search',
        'Specify search pattern',
        proc { |x, settings| settings.searchpatterns.push(Regexp.new(x)) }),
      SearchOption.new('S', 'ignorecasesearch',
        'Specify case-insensitive search pattern',
        proc { |x, settings| settings.searchpatterns.push(Regexp.new(x, true)) }),
      SearchOption.new('x', 'ext',
       'Specify extension for files to include in search',
        proc { |x, settings| settings.in_extensions.push(x) }),
      SearchOption.new('X', 'extfilter',
       'Specify extension for files to exclude from search',
        proc { |x, settings| settings.out_extensions.push(x) })
    ]
    @flag_options = [
      SearchOption.new('1', 'firstmatch',
        'Capture only the first match for a file+search combination',
        proc { |settings| settings.firstmatch = true }),
      SearchOption.new('a', 'allmatches',
        'Capture all matches*',
        proc { |settings| settings.firstmatch = false }),
      SearchOption.new('c', 'casesensitive',
        'Do case-sensitive searching*',
        proc { |settings| settings.casesensitive = true }),
      SearchOption.new('C', 'caseinsensitive',
        'Do case-insensitive searching',
        proc { |settings| settings.casesensitive = false }),
      SearchOption.new('', 'debug',
        'Set output mode to debug',
        proc { |settings| settings.debug = true }),
      SearchOption.new('h', 'help',
        'Print this usage and exit',
        proc { |settings| settings.printusage = true }),
      SearchOption.new('', 'listfiles',
       'Generate a list of the matching files after searching',
        proc { |settings| settings.listfiles = true }),
      SearchOption.new('', 'listlines',
        'Generate a list of the matching lines after searching',
        proc { |settings| settings.listlines = true }),
      SearchOption.new('m', 'multilinesearch',
        'Search files by line*',
        proc { |settings| settings.multilinesearch = true }),
      SearchOption.new('p', 'printmatches',
        'Print matches to stdout as found*',
        proc { |settings| settings.printresults = true }),
      SearchOption.new('P', 'noprintmatches',
        'Suppress printing of matches to stdout',
        proc { |settings| settings.printresults = false }),
      SearchOption.new('t', 'dotiming',
        'Time search execution',
        proc { |settings| settings.dotiming = true }),
      SearchOption.new('v', 'verbose',
        'Specify verbose output',
        proc { |settings| settings.verbose = true }),
      SearchOption.new('V', 'version',
        'Print the version and exit',
        proc { |settings| settings.printversion = true }),
      SearchOption.new('z', 'searchcompressed',
        'Search compressed files (bz2, gz, tar, zip)*',
        proc { |settings| settings.searchcompressed = true }),
      SearchOption.new('Z', 'nosearchcompressed',
        'Do not search compressed files (bz2, gz, tar, zip)',
        proc { |settings| settings.searchcompressed = false }),
    ]
  end

  def dict_from_options(opts=[])
    opt_dict = {}
    opts.each do |opt|
      if opt.shortarg != ''
        opt_dict[opt.shortarg] = opt
      end
      opt_dict[opt.longarg] = opt
    end
    opt_dict
  end

  def search_settings_from_args(args)
    settings = SearchSettings.new()
    settings.printresults = true
    while args.count > 0
      arg = args.shift
      if arg.start_with?('-')
        while arg and arg.start_with?('-')
          arg = arg[1..arg.length]
        end
        if @arg_dict.has_key?(arg)
          if args
            argval = args.shift
            @arg_dict[arg].func.call(argval, settings)
          else
            raise ArgumentError, "Missing value for option #{arg}"
          end
        elsif @flag_dict.has_key?(arg)
          @flag_dict[arg].func.call(settings)
          if ['h', 'help', 'V', 'version'].include?(arg)
            return settings
          end
        else
          raise ArgumentError, "Unknown option: #{arg}"
        end
      else
        settings.startpath = arg
      end
    end
    if settings.debug
      settings.verbose = true
    end
    settings
  end

  def usage
    puts get_usage_string
    abort
  end

  def get_usage_string
    usage = ""
    usage += ("Usage:\n")
    usage += (" rbsearch.rb [options] <startpath>\n\nOptions:\n")
    opt_strings = []
    opt_descs = []
    longest = 0
    @options.each do |opt|
      opt_string = ''
      if not opt.shortarg.empty?
        opt_string += "-#{opt.shortarg},"
      end
      opt_string += "--#{opt.longarg}"
      if opt_string.length > longest
        longest = opt_string.length
      end
      opt_strings.push(opt_string)
      opt_descs.push(opt.desc)
    end
    format_string = " %-#{longest}s  %s\n"
    i = 0
    while i < opt_strings.count
      usage += sprintf(format_string, opt_strings[i], opt_descs[i])
      i += 1
    end
    usage
  end

end
