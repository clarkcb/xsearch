################################################################################
#
# searchoptions.rb
#
# class SearchOptions: defines the available command-line options and
#                      corresponding utility methods
#
################################################################################

require 'json'
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
      :'in-archiveext' => ->(x, settings){ settings.add_exts(x,
          settings.in_archiveextensions) },
      :'in-archivefilepattern' => ->(x, settings){ settings.add_patterns(x,
        settings.in_archivefilepatterns) },
      :'in-dirpattern' => ->(x, settings){ settings.add_patterns(x,
        settings.in_dirpatterns) },
      :'in-ext' => ->(x, settings){ settings.add_exts(x,
          settings.in_extensions) },
      :'in-filepattern' => ->(x, settings){ settings.add_patterns(x,
        settings.in_filepatterns) },
      :'in-linesafterpattern' => ->(x, settings){ settings.add_patterns(x,
        settings.in_linesafterpatterns) },
      :'in-linesbeforepattern' => ->(x, settings){ settings.add_patterns(x,
        settings.in_linesbeforepatterns) },
      :'linesafter' => ->(x, settings){ settings.linesafter = x.to_i },
      :'linesaftertopattern' => ->(x, settings){ settings.add_patterns(x,
        settings.linesaftertopatterns) },
      :'linesafteruntilpattern' => ->(x, settings){ settings.add_patterns(x,
        settings.linesafteruntilpatterns) },
      :'linesbefore' => ->(x, settings){ settings.linesbefore = x.to_i },
      :'maxlinelength' => ->(x, settings){ settings.maxlinelength = x.to_i },
      :'out-archiveext' => ->(x, settings){ settings.add_exts(x,
          settings.out_archiveextensions) },
      :'out-archivefilepattern' => ->(x, settings){ settings.add_patterns(x,
        settings.out_archivefilepatterns) },
      :'out-dirpattern' => ->(x, settings){ settings.add_patterns(x,
        settings.out_dirpatterns) },
      :'out-ext' => ->(x, settings){ settings.add_exts(x,
          settings.out_extensions) },
      :'out-filepattern' => ->(x, settings){ settings.add_patterns(x,
        settings.out_filepatterns) },
      :'out-linesafterpattern' => ->(x, settings){ settings.add_patterns(x,
        settings.out_linesafterpatterns) },
      :'out-linesbeforepattern' => ->(x, settings){ settings.add_patterns(x,
        settings.out_linesbeforepatterns) },
      :'search' => ->(x, settings){ settings.add_patterns(x,
        settings.searchpatterns) },
      :'settings-file' => ->(x, settings){ settings_from_file(x, settings) }
    }
    @bool_flag_action_dict = {
      :'allmatches' => ->(b, settings){ settings.firstmatch = not(b) },
      :'archivesonly' => ->(b, settings){ settings.set_archivesonly(b) },
      :'caseinsensitive' => ->(b, settings){ settings.casesensitive = not(b) },
      :'casesensitive' => ->(b, settings){ settings.casesensitive = b },
      :'debug' => ->(b, settings){ settings.set_debug(b) },
      :'excludehidden' => ->(b, settings){ settings.excludehidden = b },
      :'firstmatch' => ->(b, settings){ settings.firstmatch = b },
      :'help' => ->(b, settings){ settings.printusage = b },
      :'includehidden' => ->(b, settings){ settings.excludehidden = not(b) },
      :'listdirs' => ->(b, settings){ settings.listdirs = b },
      :'listfiles' => ->(b, settings){ settings.listfiles = b },
      :'listlines' => ->(b, settings){ settings.listlines = b },
      :'multilinesearch' => ->(b, settings){ settings.multilinesearch = b },
      :'noprintmatches' => ->(b, settings){ settings.printresults = not(b) },
      :'norecursive' => ->(b, settings){ settings.recursive = not(b) },
      :'nosearcharchives' => ->(b, settings){ settings.searcharchives = not(b) },
      :'printmatches' => ->(b, settings){ settings.printresults = b },
      :'recursive' => ->(b, settings){ settings.recursive = b },
      :'searcharchives' => ->(b, settings){ settings.searcharchives = b },
      :'uniquelines' => ->(b, settings){ settings.uniquelines = b },
      :'verbose' => ->(b, settings){ settings.verbose = b },
      :'version' => ->(b, settings){ settings.printversion = b }
    }
    @flag_action_dict = {
      :'allmatches' => ->(settings){ settings.firstmatch = false },
      :'archivesonly' => ->(settings){ settings.set_archivesonly(true) },
      :'caseinsensitive' => ->(settings){ settings.casesensitive = false },
      :'casesensitive' => ->(settings){ settings.casesensitive = true },
      :'debug' => ->(settings){ settings.set_debug(true) },
      :'excludehidden' => ->(settings){ settings.excludehidden = true },
      :'firstmatch' => ->(settings){ settings.firstmatch = true },
      :'help' => ->(settings){ settings.printusage = true },
      :'includehidden' => ->(settings){ settings.excludehidden = false },
      :'listdirs' => ->(settings){ settings.listdirs = true },
      :'listfiles' => ->(settings){ settings.listfiles = true },
      :'listlines' => ->(settings){ settings.listlines = true },
      :'multilinesearch' => ->(settings){ settings.multilinesearch = true },
      :'noprintmatches' => ->(settings){ settings.printresults = false },
      :'norecursive' => ->(settings){ settings.recursive = false },
      :'nosearcharchives' => ->(settings){ settings.searcharchives = false },
      :'printmatches' => ->(settings){ settings.printresults = true },
      :'recursive' => ->(settings){ settings.recursive = true },
      :'searcharchives' => ->(settings){ settings.searcharchives = true },
      :'uniquelines' => ->(settings){ settings.uniquelines = true },
      :'verbose' => ->(settings){ settings.verbose = true },
      :'version' => ->(settings){ settings.printversion = true }
    }
  end

  def set_options_from_xml
    doc = Document.new(File.new(File.expand_path(SEARCHOPTIONSPATH)))
    doc.elements.each('searchoptions/searchoption') { |searchoption|
      long = searchoption.attributes['long']
      short = searchoption.attributes['short']
      desc = searchoption.text.strip
      func = nil
      if @arg_action_dict.has_key?(long.to_sym)
        func = @arg_action_dict[long.to_sym]
      elsif @flag_action_dict.has_key?(long.to_sym)
        func = @flag_action_dict[long.to_sym]
      else
        raise ArgumentError, "Unknown search option: #{long}"
      end
      option = SearchOption.new(short, long, desc, func)
      @options.push(option)
      if @arg_action_dict.has_key?(long.to_sym)
        @arg_dict[long] = option
        if short
          @arg_dict[short] = option
        end
      elsif @flag_action_dict.has_key?(long.to_sym)
        @flag_dict[long] = option
        if short
          @flag_dict[short] = option
        end
      end
    }
  end

  def settings_from_file(filepath, settings)
    # TODO: verify file exists
    json = File.read(filepath)
    settings_from_json(json, settings)
  end

  def settings_from_json(json, settings)
    json_hash = JSON.parse(json)
    json_hash.each do |arg, elem|
      if @arg_action_dict.has_key?(arg.to_sym)
        @arg_action_dict[arg.to_sym].call(json_hash[arg], settings)
      elsif @bool_flag_action_dict.has_key?(arg.to_sym)
        @bool_flag_action_dict[arg.to_sym].call(json_hash[arg], settings)
        if ['h', 'help', 'V', 'version'].include?(arg)
          return
        end
      elsif arg == 'startpath'
        settings.startpath = json_hash[arg]
      else
        raise ArgumentError, "Invalid option: #{arg}"
      end
    end
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
          if args.count > 0
            argval = args.shift
            @arg_dict[arg].func.call(argval, settings)
          else
            raise ArgumentError, "Missing value for option #{arg}"
          end
        elsif @flag_dict.has_key?(arg)
          puts "arg in @flag_dict\n"
          @flag_dict[arg].func.call(settings)
          if ['h', 'help', 'V', 'version'].include?(arg)
            return settings
          end
        else
          raise ArgumentError, "Invalid option: #{arg}"
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
