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
        proc { |x, settings| puts "numlinesbefore: #{x}" },
        'Number of lines to show before every matched line (default: 0)'),
      SearchOption.new('B', 'numlinesafter',
        proc { |x, settings| puts "numlinesafter: #{x}" },
        'Number of lines to show after every matched line (default: 0)'),
      SearchOption.new('d', 'dirname',
        proc { |x, settings| settings.in_dirpatterns.push(Regexp.new(x)) },
        'Specify name pattern for directories to include in search'),
      SearchOption.new('D', 'dirfilter',
        proc { |x, settings| settings.out_dirpatterns.push(Regexp.new(x)) },
        'Specify name pattern for directories to exclude from search'),
      SearchOption.new('f', 'filename',
        proc { |x, settings| settings.in_filepatterns.push(Regexp.new(x)) },
        'Specify name pattern for files to include in search'),
      SearchOption.new('F', 'filefilter',
        proc { |x, settings| settings.out_filepatterns.push(Regexp.new(x)) },
        'Specify name pattern for files to exclude from search'),
      SearchOption.new('', 'ignorecasesearchfile',
        proc { |x, settings| puts "ingnorecasesearchfile: #{x}" },
        'Specify file containing case-insensitive search patterns (one per line)'),
      SearchOption.new('', 'linesafterfilter',
        proc { |x, settings| puts "linesafterfilter: #{x}" },
        'Specify pattern to filter the "lines-after" lines on (used with --numlinesafter)'),
      SearchOption.new('', 'linesaftersearch',
        proc { |x, settings| puts "linesaftersearch: #{x}" },
        'Specify pattern to search the "lines-after" lines on (used with --numlinesafter)'),
      SearchOption.new('', 'linesbeforefilter',
        proc { |x, settings| puts "linesbeforefilter: #{x}" },
        'Specify pattern to filter the "lines-before" lines on (used with --numlinesbefore)'),
      SearchOption.new('', 'linesbeforesearch',
        proc { |x, settings| puts "linesbeforesearch: #{x}" },
        'Specify pattern to search the "lines-before" lines on (used with --numlinesbefore)'),
      SearchOption.new('s', 'search',
        proc { |x, settings| settings.searchpatterns.push(Regexp.new(x)) },
        'Specify search pattern'),
      SearchOption.new('S', 'ignorecasesearch',
        proc { |x, settings| settings.searchpatterns.push(Regexp.new(x, true)) },
        'Specify case-insensitive search pattern'),
      SearchOption.new('', 'searchfile',
        proc { |x, settings| puts "searchfile: #{x}" },
        'Specify file containing search patterns (one per line)'),
      SearchOption.new('x', 'ext',
        proc { |x, settings| settings.in_extensions.push(x) },
       'Specify extension for files to include in search'),
      SearchOption.new('X', 'extfilter',
        proc { |x, settings| settings.out_extensions.push(x) },
       'Specify extension for files to exclude from search')
    ]
    @flag_options = [
      SearchOption.new('1', 'firstmatch',
        proc { |settings| settings.firstmatch = true },
        'Capture only the first match for a file+search combination'),
      SearchOption.new('a', 'allmatches',
        proc { |settings| settings.firstmatch = false },
        'Capture all matches*'),
      SearchOption.new('c', 'casesensitive',
        proc { |settings| settings.casesensitive = true },
        'Do case-sensitive searching*'),
      SearchOption.new('C', 'caseinsensitive',
        proc { |settings| settings.casesensitive = false },
        'Do case-insensitive searching'),
      SearchOption.new('', 'debug',
        proc { |settings| settings.debug = true },
        'Set output mode to debug'),
      SearchOption.new('h', 'help',
        proc { |settings| settings.printusage = true },
        'Print this usage and exit'),
      SearchOption.new('', 'listfiles',
        proc { |settings| settings.listfiles = true },
       'Generate a list of the matching files after searching'),
      SearchOption.new('', 'listlines',
        proc { |settings| settings.listlines = true },
        'Generate a list of the matching lines after searching'),
      SearchOption.new('m', 'multilinesearch',
        proc { |settings| settings.multilinesearch = true },
        'Search files by line*'),
      SearchOption.new('p', 'printmatches',
        proc { |settings| settings.printresults = true },
        'Print matches to stdout as found*'),
      SearchOption.new('P', 'noprintmatches',
        proc { |settings| settings.printresults = false },
        'Suppress printing of matches to stdout'),
      SearchOption.new('t', 'dotiming',
        proc { |settings| settings.dotiming = true },
        'Time search execution'),
      SearchOption.new('v', 'verbose',
        proc { |settings| settings.verbose = true },
        'Specify verbose output'),
      SearchOption.new('V', 'version',
        proc { |settings| settings.printversion = true },
        'Print the version and exit'),
      SearchOption.new('z', 'searchcompressed',
        proc { |settings| settings.searchcompressed = true },
        'Search compressed files (bz2, gz, tar, zip)*'),
      SearchOption.new('Z', 'nosearchcompressed',
        proc { |settings| settings.searchcompressed = false },
        'Do not search compressed files (bz2, gz, tar, zip)'),
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
    if not settings.startpath
      raise ArgumentError, 'Missing startpath'
    end
    if settings.searchpatterns.count < 1
      raise ArgumentError, 'No search patterns specified'
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
