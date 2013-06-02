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
require 'searchoption.rb'
require 'searchsettings.rb'

class SearchOptions
  
  def initialize
    # TODO: move to a config file
    @searchoptionspath = '/Users/cary/src/git/xsearch/shared/searchoptions.xml'
    @options = []
    @arg_dict = {}
    @flag_dict = {}
    set_actions
    set_options_from_xml
    @options.sort! { |a, b| a.sortarg <=> b.sortarg }
  end

  def set_actions
    @arg_action_dict = {
      'in-dirpattern' =>
        proc { |x, settings| settings.in_dirpatterns.push(Regexp.new(x)) },
      'in-ext' =>
        proc { |x, settings| settings.in_extensions.push(x) },
      'in-filepattern' =>
        proc { |x, settings| settings.in_filepatterns.push(Regexp.new(x)) },
      'in-linesafterpattern' =>
        proc { |x, settings| puts "in-linesafterpattern: #{x}" },
      'in-linesbeforepattern' =>
        proc { |x, settings| puts "in-linesbeforepattern: #{x}" },
      'linesafter' =>
        proc { |x, settings| puts "linesafter: #{x}" },
      'linesbefore' =>
        proc { |x, settings| puts "linesbefore: #{x}" },
      'out-dirpattern' =>
        proc { |x, settings| settings.out_dirpatterns.push(Regexp.new(x)) },
      'out-ext' =>
        proc { |x, settings| settings.out_extensions.push(x) },
      'out-filepattern' =>
        proc { |x, settings| settings.out_filepatterns.push(Regexp.new(x)) },
      'out-linesafterpattern' =>
        proc { |x, settings| puts "out-linesafterpattern: #{x}" },
      'out-linesbeforepattern' =>
        proc { |x, settings| puts "out-linesbeforepattern: #{x}" },
      'search' =>
        proc { |x, settings| settings.searchpatterns.push(Regexp.new(x)) }
    }
    @flag_action_dict = {
      'allmatches' =>
        proc { |settings| settings.firstmatch = false },
      'caseinsensitive' =>
        proc { |settings| settings.casesensitive = false },
      'casesensitive' =>
        proc { |settings| settings.casesensitive = true },
      'debug' =>
        proc { |settings| settings.debug = true },
      'dotiming' =>
        proc { |settings| settings.dotiming = true },
      'firstmatch' =>
        proc { |settings| settings.firstmatch = true },
      'help' =>
        proc { |settings| settings.printusage = true },
      'listfiles' =>
        proc { |settings| settings.listfiles = true },
      'listlines' =>
        proc { |settings| settings.listlines = true },
      'multilinesearch' =>
        proc { |settings| settings.multilinesearch = true },
      'noprintmatches' =>
        proc { |settings| settings.printresults = false },
      'nosearchcompressed' =>
        proc { |settings| settings.searchcompressed = false },
      'printmatches' =>
        proc { |settings| settings.printresults = true },
      'searchcompressed' =>
        proc { |settings| settings.searchcompressed = true },
      'verbose' =>
        proc { |settings| settings.verbose = true },
      'version' =>
        proc { |settings| settings.printversion = true }
    }
  end

  def set_options_from_xml
    doc = Document.new(File.new(@searchoptionspath))
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
