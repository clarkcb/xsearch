# SearchOptions - generate help, create settings from CLI args
require 'json'

require 'rbfind/common'
require_relative 'searcherror'
require_relative 'searchoption'
require_relative 'searchsettings'

module RbSearch

  # SearchOptions - parses CLI args into settings, generates usage string
  class SearchOptions

    def initialize
      @options = []
      @arg_action_dict = {}
      @bool_flag_action_dict = {}
      @long_arg_dict = {}
      set_actions
      set_options_from_json
      @options.sort! { |a, b| a.sort_arg <=> b.sort_arg }
    end

    def search_settings_from_args(args)
      settings = SearchSettings.new
      settings.print_results = true
      until args.empty?
        arg = args.shift
        if arg.start_with?('-')
          arg = arg[1..arg.length] while arg && arg.start_with?('-')
          long_arg = @long_arg_dict[arg]
          if @arg_action_dict.key?(long_arg)
            raise SearchError, "Missing value for option #{arg}" if args.empty?
            arg_val = args.shift
            @arg_action_dict[long_arg].call(arg_val, settings)
          elsif @bool_flag_action_dict.key?(long_arg)
            @bool_flag_action_dict[long_arg].call(true, settings)
            return settings if %w[help version].include?(long_arg)
          else
            raise SearchError, "Invalid option: #{arg}"
          end
        else
          settings.paths.push(arg)
        end
      end
      settings
    end

    def settings_from_file(file_path, settings)
      f = File.open(file_path, mode: 'r')
      json = f.read
      settings_from_json(json, settings)
    rescue IOError => e
      raise SearchError, "#{e} (file: #{file_path})"
    rescue ArgumentError => e
      raise SearchError, "#{e} (file: #{file_path})"
    rescue SearchError => e
      raise SearchError, "#{e} (file: #{file_path})"
    ensure
      f&.close
    end

    def settings_from_json(json, settings)
      json_hash = JSON.parse(json)
      json_hash.each_key do |arg|
        arg_sym = arg.to_sym
        if @arg_action_dict.key?(arg_sym)
          @arg_action_dict[arg_sym].call(json_hash[arg], settings)
        elsif @bool_flag_action_dict.key?(arg_sym)
          @bool_flag_action_dict[arg_sym].call(json_hash[arg], settings)
          return if %w[h help V version].include?(arg)
        elsif arg == 'path'
          settings.paths.push(json_hash[arg])
        else
          raise SearchError, "Invalid option: #{arg}"
        end
      end
    end

    def usage
      puts "#{get_usage_string}\n"
      abort
    end

    def get_usage_string
      usage = "Usage:\n"
      usage << " rbsearch [options] -s <searchpattern> <path> [<path> ...]\n\n"
      usage << "Options:\n"
      opt_strings = []
      opt_descs = []
      longest = 0
      @options.each do |opt|
        opt_string = ''
        opt_string << "-#{opt.short_arg}," unless opt.short_arg.empty?
        opt_string << "--#{opt.long_arg}"
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

    def set_actions
      @arg_action_dict = {
        'encoding': lambda { |x, settings|
          settings.text_file_encoding = x
        },
        'in-archiveext': lambda { |x, settings|
          settings.add_exts(x, settings.in_archive_extensions)
        },
        'in-archivefilepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_archive_file_patterns)
        },
        'in-dirpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_dir_patterns)
        },
        'in-ext': lambda { |x, settings|
          settings.add_exts(x, settings.in_extensions)
        },
        'in-filetype': lambda { |x, settings|
          settings.add_file_types(x, settings.in_file_types)
        },
        'in-filepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_file_patterns)
        },
        'in-linesafterpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_lines_after_patterns)
        },
        'in-linesbeforepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_lines_before_patterns)
        },
        'linesafter': lambda { |x, settings|
          settings.lines_after = x.to_i
        },
        'linesaftertopattern': lambda { |x, settings|
          settings.add_patterns(x, settings.lines_after_to_patterns)
        },
        'linesafteruntilpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.lines_after_until_patterns)
        },
        'linesbefore': lambda { |x, settings|
          settings.lines_before = x.to_i
        },
        'maxdepth': lambda { |x, settings|
          settings.max_depth = x.to_i
        },
        'maxlastmod': lambda { |x, settings|
          settings.max_last_mod = DateTime.parse(x)
        },
        'maxlinelength': lambda { |x, settings|
          settings.max_line_length = x.to_i
        },
        'maxsize': lambda { |x, settings|
          settings.max_size = x.to_i
        },
        'mindepth': lambda { |x, settings|
          settings.min_depth = x.to_i
        },
        'minlastmod': lambda { |x, settings|
          settings.min_last_mod = DateTime.parse(x)
        },
        'minsize': lambda { |x, settings|
          settings.min_size = x.to_i
        },
        'out-archiveext': lambda { |x, settings|
          settings.add_exts(x, settings.out_archive_extensions)
        },
        'out-archivefilepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_archive_file_patterns)
        },
        'out-dirpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_dir_patterns)
        },
        'out-ext': lambda { |x, settings|
          settings.add_exts(x, settings.out_extensions)
        },
        'out-filepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_file_patterns)
        },
        'out-filetype': lambda { |x, settings|
          settings.add_file_types(x, settings.out_file_types)
        },
        'out-linesafterpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_lines_after_patterns)
        },
        'out-linesbeforepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_lines_before_patterns)
        },
        'path': lambda { |x, settings|
          settings.paths.push(x)
        },
        'searchpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.search_patterns)
        },
        'settings-file': lambda { |x, settings|
          settings_from_file(x, settings)
        },
        'sort-by': lambda { |x, settings|
          settings.set_sort_by(x)
        }
      }
      @bool_flag_action_dict = {
        allmatches: ->(b, settings) { settings.first_match = !b },
        archivesonly: ->(b, settings) { settings.archives_only = b },
        colorize: ->(b, settings) { settings.colorize = b },
        debug: ->(b, settings) { settings.debug = b },
        excludehidden: ->(b, settings) { settings.exclude_hidden = b },
        firstmatch: ->(b, settings) { settings.first_match = b },
        help: ->(b, settings) { settings.print_usage = b },
        includehidden: ->(b, settings) { settings.exclude_hidden = !b },
        listdirs: ->(b, settings) { settings.list_dirs = b },
        listfiles: ->(b, settings) { settings.list_files = b },
        listlines: ->(b, settings) { settings.list_lines = b },
        multilinesearch: ->(b, settings) { settings.multi_line_search = b },
        nocolorize: ->(b, settings) { settings.colorize = !b },
        noprintmatches: ->(b, settings) { settings.print_results = !b },
        norecursive: ->(b, settings) { settings.recursive = !b },
        nosearcharchives: ->(b, settings) { settings.search_archives = !b },
        printmatches: ->(b, settings) { settings.print_results = b },
        recursive: ->(b, settings) { settings.recursive = b },
        searcharchives: ->(b, settings) { settings.search_archives = b },
        'sort-ascending': ->(b, settings) { settings.sort_descending = !b },
        'sort-caseinsensitive': ->(b, settings) { settings.sort_case_insensitive = b },
        'sort-casesensitive': ->(b, settings) { settings.sort_case_insensitive = !b },
        'sort-descending': ->(b, settings) { settings.sort_descending = b },
        uniquelines: ->(b, settings) { settings.unique_lines = b },
        verbose: ->(b, settings) { settings.verbose = b },
        version: ->(b, settings) { settings.print_version = b }
      }
      @long_arg_dict = {}
    end

    def set_options_from_json
      search_options_json_path = File.join(File.dirname(__FILE__), "../../data/searchoptions.json")
      f = File.open(search_options_json_path, mode: 'r')
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
        func =
          if @arg_action_dict.key?(long_sym)
            @arg_action_dict[long_sym]
          elsif @bool_flag_action_dict.key?(long_sym)
            @bool_flag_action_dict[long_sym]
          else
            raise SearchError, "Unknown search option: #{long}"
          end
        @options.push(SearchOption.new(short, long, desc, func))
        @long_arg_dict[long] = long_sym
        @long_arg_dict[short] = long_sym if short
      end
    rescue StandardError => e
      raise SearchError, "#{e} (file: #{SEARCHOPTIONSJSONPATH})"
    ensure
      f&.close
    end
  end
end
