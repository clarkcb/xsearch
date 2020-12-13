# SearchOptions - generate help, create settings from CLI args
require 'json'

require_relative 'common'
require_relative 'searcherror'
require_relative 'searchoption'
require_relative 'searchsettings'

# SearchOptions - parses CLI args into settings, generates usage string
class SearchOptions

  def initialize
    @default_file_name = '.xsearch.json'
    @options = []
    @arg_action_dict = {}
    @bool_arg_dict = {}
    @coll_arg_dict = {}
    @longarg_dict = {}
    set_dicts
    set_options_from_json
    # set_options_from_xml
    @options.sort! { |a, b| a.sortarg <=> b.sortarg }
  end

  def search_settings_from_args(args)
    settings = SearchSettings.new
    settings.printresults = true
    arg_dict = {}
    until args.empty?
      arg = args.shift
      if arg.start_with?('-')
        arg = arg[1..arg.length] while arg && arg.start_with?('-')
        longarg = @longarg_dict[arg]
        if @bool_arg_dict.key?(longarg)
          if %w[help version].include?(longarg)
            @bool_arg_dict[longarg].call(true, settings)
            return settings if %w[help version].include?(longarg)
          else
            arg_dict[longarg] = true
          end
        elsif @coll_arg_dict.key?(longarg) ||  @int_arg_dict.key?(longarg) ||  @str_arg_dict.key?(longarg)
          raise SearchError, "Missing value for option #{arg}" if args.empty?
          argval = args.shift
          if @coll_arg_dict.key?(longarg)
            arg_dict[longarg] = argval
          elsif @int_arg_dict.key?(longarg)
            argint = Integer(argval)
            if argint < 0
              raise SearchError, "Invalid value for #{arg}: #{argval}"
            end
            arg_dict[longarg] = argint
          elsif @str_arg_dict.key?(longarg)
            arg_dict[longarg] = argval
          end
        else
          raise SearchError, "Invalid option: #{arg}"
        end
      else
        arg_dict['startpath'] = arg
      end
    end
    unless arg_dict.has_key?('nodefaultsfiles')
      load_default_files(arg_dict['startpath'], settings)
    end
    settings_from_hash(arg_dict, settings)
    settings
  rescue ArgumentError => e
    raise SearchError, "#{e}"
  end

  def settings_from_hash(arg_hash, settings)
    arg_hash.each_key do |arg|
      arg_sym = arg.to_sym
      if @bool_arg_dict.key?(arg_sym)
        @bool_arg_dict[arg_sym].call(arg_hash[arg], settings)
        return if %w[h help V version].include?(arg)
      elsif @coll_arg_dict.key?(arg_sym)
        if arg_hash[arg].is_a?(Array)
          arg_hash[arg].each do |a|
            @coll_arg_dict[arg_sym].call(a, settings)
          end
        else
          @coll_arg_dict[arg_sym].call(arg_hash[arg], settings)
        end
      elsif @int_arg_dict.key?(arg_sym)
        @int_arg_dict[arg_sym].call(arg_hash[arg], settings)
      elsif @str_arg_dict.key?(arg_sym)
        @str_arg_dict[arg_sym].call(arg_hash[arg], settings)
      else
        raise SearchError, "Invalid option: #{arg}"
      end
    end
  end

  def settings_from_json(json, settings)
    json_hash = JSON.parse(json)
    settings_from_hash(json_hash, settings)
  end

  def settings_from_file(filepath, settings)
    f = File.open(filepath, mode: 'r')
    json = f.read
    settings_from_json(json, settings)
  rescue IOError => e
    raise SearchError, "#{e} (file: #{filepath})"
  rescue ArgumentError => e
    raise SearchError, "#{e} (file: #{filepath})"
  rescue SearchError => e
    raise SearchError, "#{e} (file: #{filepath})"
  ensure
    f&.close
  end

  def load_default_files(startpath, settings)
    if !startpath.nil? && Pathname.new(startpath).exist? && (Pathname.new(startpath) + @default_file_name).exist?
      startpath_default_file = (Pathname.new(startpath) + @default_file_name).to_s
      puts("#{startpath_default_file} file found")
      settings_from_file(startpath_default_file, settings)
    end
    if (Pathname.new(File.expand_path("~")) + @default_file_name).exist?
      home_default_file = (Pathname.new(File.expand_path("~")) + @default_file_name).to_s
      puts("#{home_default_file} file found")
      settings_from_file(home_default_file, settings)
    end
  end

  def usage
    puts "#{get_usage_string}\n"
    abort
  end

  def get_usage_string
    usage = "Usage:\n"
    usage << " rbsearch [options] -s <searchpattern> <startpath>\n\n"
    usage << "Options:\n"
    opt_strings = []
    opt_descs = []
    longest = 0
    @options.each do |opt|
      opt_string = ''
      opt_string << "-#{opt.shortarg}," unless opt.shortarg.empty?
      opt_string << "--#{opt.longarg}"
      longest = opt_string.length > longest ? opt_string.length : longest
      opt_strings.push(opt_string)
      opt_descs.push(opt.desc)
    end
    format_string = " %-#{longest}s  %s\n"
    i = 0
    while i < opt_strings.size
      usage << format(format_string, opt_strings[i], opt_descs[i])
      i += 1
    end
    usage
  end

  private

  def set_dicts
    @bool_arg_dict = {
      allmatches: ->(b, settings) { settings.firstmatch = !b },
      archivesonly: ->(b, settings) { settings.archivesonly = b },
      caseinsensitive: ->(b, settings) { settings.casesensitive = !b },
      casesensitive: ->(b, settings) { settings.casesensitive = b },
      colorize: ->(b, settings) { settings.colorize = b },
      debug: ->(b, settings) { settings.debug = b },
      defaultsfiles: ->(b, settings) { settings.defaultsfiles = b },
      excludehidden: ->(b, settings) { settings.excludehidden = b },
      firstmatch: ->(b, settings) { settings.firstmatch = b },
      help: ->(b, settings) { settings.printusage = b },
      includehidden: ->(b, settings) { settings.excludehidden = !b },
      listdirs: ->(b, settings) { settings.listdirs = b },
      listfiles: ->(b, settings) { settings.listfiles = b },
      listlines: ->(b, settings) { settings.listlines = b },
      multilinesearch: ->(b, settings) { settings.multilinesearch = b },
      nocolorize: ->(b, settings) { settings.colorize = !b },
      nodefaultsfiles: ->(b, settings) { settings.defaultsfiles = !b },
      noprintmatches: ->(b, settings) { settings.printresults = !b },
      norecursive: ->(b, settings) { settings.recursive = !b },
      nosearcharchives: ->(b, settings) { settings.searcharchives = !b },
      printmatches: ->(b, settings) { settings.printresults = b },
      recursive: ->(b, settings) { settings.recursive = b },
      searcharchives: ->(b, settings) { settings.searcharchives = b },
      uniquelines: ->(b, settings) { settings.uniquelines = b },
      verbose: ->(b, settings) { settings.verbose = b },
      version: ->(b, settings) { settings.printversion = b }
    }
    @coll_arg_dict = {
      'in-archiveext': lambda { |x, settings|
        settings.add_exts(x, settings.in_archiveextensions)
      },
      'in-archivefilepattern': lambda { |x, settings|
        settings.add_patterns(x, settings.in_archivefilepatterns)
      },
      'in-dirpattern': lambda { |x, settings|
        settings.add_patterns(x, settings.in_dirpatterns)
      },
      'in-ext': lambda { |x, settings|
        settings.add_exts(x, settings.in_extensions)
      },
      'in-filetype': lambda { |x, settings|
        settings.add_filetypes(x, settings.in_filetypes)
      },
      'in-filepattern': lambda { |x, settings|
        settings.add_patterns(x, settings.in_filepatterns)
      },
      'in-linesafterpattern': lambda { |x, settings|
        settings.add_patterns(x, settings.in_linesafterpatterns)
      },
      'in-linesbeforepattern': lambda { |x, settings|
        settings.add_patterns(x, settings.in_linesbeforepatterns)
      },
      'linesaftertopattern': lambda { |x, settings|
        settings.add_patterns(x, settings.linesaftertopatterns)
      },
      'linesafteruntilpattern': lambda { |x, settings|
        settings.add_patterns(x, settings.linesafteruntilpatterns)
      },
      'out-archiveext': lambda { |x, settings|
        settings.add_exts(x, settings.out_archiveextensions)
      },
      'out-archivefilepattern': lambda { |x, settings|
        settings.add_patterns(x, settings.out_archivefilepatterns)
      },
      'out-dirpattern': lambda { |x, settings|
        settings.add_patterns(x, settings.out_dirpatterns)
      },
      'out-ext': lambda { |x, settings|
        settings.add_exts(x, settings.out_extensions)
      },
      'out-filepattern': lambda { |x, settings|
        settings.add_patterns(x, settings.out_filepatterns)
      },
      'out-filetype': lambda { |x, settings|
        settings.add_filetypes(x, settings.out_filetypes)
      },
      'out-linesafterpattern': lambda { |x, settings|
        settings.add_patterns(x, settings.out_linesafterpatterns)
      },
      'out-linesbeforepattern': lambda { |x, settings|
        settings.add_patterns(x, settings.out_linesbeforepatterns)
      },
      'searchpattern': lambda { |x, settings|
        settings.add_patterns(x, settings.searchpatterns)
      }
    }
    @int_arg_dict = {
      'linesafter': lambda { |x, settings|
        settings.linesafter = x.to_i
      },
      'linesbefore': lambda { |x, settings|
        settings.linesbefore = x.to_i
      },
      'maxlinelength': lambda { |x, settings|
        settings.maxlinelength = x.to_i
      },
    }
    @str_arg_dict = {
      'encoding': lambda { |s, settings|
        settings.textfileencoding = s
      },
      'settings-file': lambda { |s, settings|
        settings_from_file(s, settings)
      },
      'startpath': lambda { |s, settings|
        settings.startpath = s
      }
    }
    @longarg_dict = {}
  end

  def set_options_from_json
    searchoptions_json_path = File.join(File.dirname(__FILE__), "../../data/searchoptions.json")
    f = File.open(searchoptions_json_path, mode: 'r')
    json = f.read
    json_hash = JSON.parse(json)
    json_hash['searchoptions'].each do |so|
      long = so['long']
      short =
        if so.key?('short')
          so['short']
        else
          ''
        end
      desc = so['desc']
      long_sym = long.to_sym
      # TODO: remove func from SearchOption, not being used and doesn't really provide additional benefit
      # func =
      #   if @bool_arg_dict.key?(long_sym)
      #     @bool_arg_dict[long_sym]
      #   elsif @coll_arg_dict.key?(long_sym)
      #     @coll_arg_dict[long_sym]
      #   elsif @int_arg_dict.key?(long_sym)
      #     @int_arg_dict[long_sym]
      #   elsif @str_arg_dict.key?(long_sym)
      #     @str_arg_dict[long_sym]
      #   else
      #     raise SearchError, "Unknown search option: #{long}"
      #   end
      func = nil
      @options.push(SearchOption.new(short, long, desc, func))
      @longarg_dict[long] = long_sym
      @longarg_dict[short] = long_sym if short
    end
  rescue StandardError => e
    raise SearchError, "#{e} (file: #{searchoptions_json_path})"
  ensure
    f&.close
  end

end
