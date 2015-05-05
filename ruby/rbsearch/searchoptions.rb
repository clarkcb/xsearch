################################################################################
#
# searchoptions.rb
#
# class SearchOptions: defines the available command-line options and
#                      corresponding utility methods
#
################################################################################

require 'rexml/document'
include REXML
require_relative 'config.rb'
require_relative 'searchoption.rb'
require_relative 'searchsettings.rb'

class SearchOptions
  
  def initialize
    @options = []
    @arg_dict = {}
    @flag_dict = {}
    set_actions
    set_options_from_xml
    @options.sort! { |a, b| a.sortarg <=> b.sortarg }
  end

  def set_actions
    @arg_action_dict = {
      'in-archiveext' => ->(x, settings){ settings.add_comma_delimited_exts(x,
          settings.in_archiveextensions) },
      'in-archivefilepattern' => ->(x, settings){ settings.add_pattern(x,
        settings.in_archivefilepatterns) },
      'in-dirpattern' => ->(x, settings){ settings.add_pattern(x,
        settings.in_dirpatterns) },
      'in-ext' => ->(x, settings){ settings.add_comma_delimited_exts(x,
          settings.in_extensions) },
      'in-filepattern' => ->(x, settings){ settings.add_pattern(x,
        settings.in_filepatterns) },
      'in-linesafterpattern' => ->(x, settings){ settings.add_pattern(x,
        settings.in_linesafterpatterns) },
      'in-linesbeforepattern' => ->(x, settings){ settings.add_pattern(x,
        settings.in_linesbeforepatterns) },
      'linesafter' => ->(x, settings){ settings.linesafter = x.to_i },
      'linesaftertopattern' => ->(x, settings){ settings.add_pattern(x,
        settings.linesaftertopatterns) },
      'linesafteruntilpattern' => ->(x, settings){ settings.add_pattern(x,
        settings.linesafteruntilpatterns) },
      'linesbefore' => ->(x, settings){ settings.linesbefore = x.to_i },
      'maxlinelength' => ->(x, settings){ settings.maxlinelength = x.to_i },
      'out-archiveext' => ->(x, settings){ settings.add_comma_delimited_exts(x,
          settings.out_archiveextensions) },
      'out-archivefilepattern' => ->(x, settings){ settings.add_pattern(x,
        settings.out_archivefilepatterns) },
      'out-dirpattern' => ->(x, settings){ settings.add_pattern(x,
        settings.out_dirpatterns) },
      'out-ext' => ->(x, settings){ settings.add_comma_delimited_exts(x,
          settings.out_extensions) },
      'out-filepattern' => ->(x, settings){ settings.add_pattern(x,
        settings.out_filepatterns) },
      'out-linesafterpattern' => ->(x, settings){ settings.add_pattern(x,
        settings.out_linesafterpatterns) },
      'out-linesbeforepattern' => ->(x, settings){ settings.add_pattern(x,
        settings.out_linesbeforepatterns) },
      'search' => ->(x, settings){ settings.add_pattern(x,
        settings.searchpatterns) }
    }
    @flag_action_dict = {
      'allmatches' => ->(settings){ settings.firstmatch = false },
      'archivesonly' => ->(settings){ settings.archivesonly = true;
        settings.searcharchives = true },
      'caseinsensitive' => ->(settings){ settings.casesensitive = false },
      'casesensitive' => ->(settings){ settings.casesensitive = true },
      'debug' => ->(settings){ settings.debug = true; settings.verbose = true },
      'dotiming' => ->(settings){ settings.dotiming = true },
      'excludehidden' => ->(settings){ settings.excludehidden = true },
      'firstmatch' => ->(settings){ settings.firstmatch = true },
      'help' => ->(settings){ settings.printusage = true },
      'includehidden' => ->(settings){ settings.excludehidden = false },
      'listdirs' => ->(settings){ settings.listdirs = true },
      'listfiles' => ->(settings){ settings.listfiles = true },
      'listlines' => ->(settings){ settings.listlines = true },
      'multilinesearch' => ->(settings){ settings.multilinesearch = true },
      'noprintmatches' => ->(settings){ settings.printresults = false },
      'norecursive' => ->(settings){ settings.recursive = false },
      'nosearcharchives' => ->(settings){ settings.searcharchives = false },
      'printmatches' => ->(settings){ settings.printresults = true },
      'recursive' => ->(settings){ settings.recursive = true },
      'searcharchives' => ->(settings){ settings.searcharchives = true },
      'uniquelines' => ->(settings){ settings.uniquelines = true },
      'verbose' => ->(settings){ settings.verbose = true },
      'version' => ->(settings){ settings.printversion = true }
    }
  end

  def set_options_from_xml
    doc = Document.new(File.new(File.expand_path(SEARCHOPTIONSPATH)))
    doc.elements.each('searchoptions/searchoption') { |searchoption|
      long = searchoption.attributes['long']
      short = searchoption.attributes['short']
      desc = searchoption.text.strip
      func = nil
      if @arg_action_dict.has_key?(long)
        func = @arg_action_dict[long]
      elsif @flag_action_dict.has_key?(long)
        func = @flag_action_dict[long]
      else
        raise ArgumentError, "Unknown search option: #{long}"
      end
      option = SearchOption.new(short, long, desc, func)
      @options.push(option)
      if @arg_action_dict.has_key?(long)
        @arg_dict[long] = option
        if short
          @arg_dict[short] = option
        end
      elsif @flag_action_dict.has_key?(long)
        @flag_dict[long] = option
        if short
          @flag_dict[short] = option
        end
      end
    }
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
    settings
  end

  def usage
    puts "#{get_usage_string}\n"
    abort
  end

  def get_usage_string
    usage = ""
    usage += ("Usage:\n")
    usage += (" rbsearch.rb [options] -s <searchpattern> <startpath>\n\nOptions:\n")
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
