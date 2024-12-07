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
      @bool_action_dict = {}
      @str_action_dict = {}
      @int_action_dict = {}
      @long_arg_dict = {}
      set_actions
      set_options_from_json
      @options.sort! { |a, b| a.sort_arg <=> b.sort_arg }
    end

    def search_settings_from_args(args)
      settings = SearchSettings.new
      # default print_files to true since running as cli
      settings.print_results = true
      until args.empty?
        arg = args.shift
        if arg.start_with?('-')
          arg = arg[1..arg.length] while arg && arg.start_with?('-')
          long_arg = @long_arg_dict[arg]
          if @bool_action_dict.key?(long_arg)
            @bool_action_dict[long_arg].call(true, settings)
            return settings if [:help, :version].include?(long_arg)
            elsif @str_action_dict.key?(long_arg) || @int_action_dict.key?(long_arg)
            raise SearchError, "Missing value for option #{arg}" if args.empty?
            arg_val = args.shift
            if @str_action_dict.key?(long_arg)
              @str_action_dict[long_arg].call(arg_val, settings)
            else
              @int_action_dict[long_arg].call(arg_val.to_i, settings)
            end
          else
            raise SearchError, "Invalid option: #{arg}"
          end
        else
          settings.add_path(arg)
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
        if @bool_action_dict.key?(arg_sym)
          @bool_action_dict[arg_sym].call(json_hash[arg], settings)
          return if %w[help version].include?(arg)
        elsif @str_action_dict.key?(arg_sym)
          @str_action_dict[arg_sym].call(json_hash[arg], settings)
        elsif @int_action_dict.key?(arg_sym)
          @int_action_dict[arg_sym].call(json_hash[arg], settings)
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
      @bool_action_dict = {
        allmatches: ->(b, settings) { settings.first_match = !b },
        archivesonly: ->(b, settings) { settings.archives_only = b },
        colorize: ->(b, settings) { settings.colorize = b },
        debug: ->(b, settings) { settings.debug = b },
        excludehidden: ->(b, settings) { settings.include_hidden = !b },
        firstmatch: ->(b, settings) { settings.first_match = b },
        followsymlinks: ->(b, settings) { settings.follow_symlinks = b },
        help: ->(b, settings) { settings.print_usage = b },
        includehidden: ->(b, settings) { settings.include_hidden = b },
        multilinesearch: ->(b, settings) { settings.multi_line_search = b },
        nocolorize: ->(b, settings) { settings.colorize = !b },
        nofollowsymlinks: ->(b, settings) { settings.follow_symlinks = !b },
        noprintdirs: ->(b, settings) { settings.print_dirs = !b },
        noprintfiles: ->(b, settings) { settings.print_files = !b },
        noprintlines: ->(b, settings) { settings.print_lines = !b },
        noprintmatches: ->(b, settings) { settings.print_results = !b },
        norecursive: ->(b, settings) { settings.recursive = !b },
        nosearcharchives: ->(b, settings) { settings.search_archives = !b },
        printdirs: ->(b, settings) { settings.print_dirs = b },
        printfiles: ->(b, settings) { settings.print_files = b },
        printlines: ->(b, settings) { settings.print_lines = b },
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
      @str_action_dict = {
        encoding: ->(s, settings) { settings.text_file_encoding = s },
        'in-archiveext': ->(s, settings) { settings.add_exts(s, settings.in_archive_extensions) },
        'in-archivefilepattern': ->(s, settings) { settings.add_patterns(s, settings.in_archive_file_patterns) },
        'in-dirpattern': ->(s, settings) { settings.add_patterns(s, settings.in_dir_patterns) },
        'in-ext': ->(s, settings) { settings.add_exts(s, settings.in_extensions) },
        'in-filetype': ->(s, settings) { settings.add_file_types(s, settings.in_file_types) },
        'in-filepattern': ->(s, settings) { settings.add_patterns(s, settings.in_file_patterns) },
        'in-linesafterpattern': ->(s, settings) { settings.add_patterns(s, settings.in_lines_after_patterns) },
        'in-linesbeforepattern': ->(s, settings) { settings.add_patterns(s, settings.in_lines_before_patterns) },
        linesaftertopattern: ->(s, settings) { settings.add_patterns(s, settings.lines_after_to_patterns) },
        linesafteruntilpattern: ->(s, settings) { settings.add_patterns(s, settings.lines_after_until_patterns) },
        maxlastmod: ->(s, settings) { settings.max_last_mod = DateTime.parse(s) },
        minlastmod: ->(s, settings) { settings.min_last_mod = DateTime.parse(s) },
        'out-archiveext': ->(s, settings) { settings.add_exts(s, settings.out_archive_extensions) },
        'out-archivefilepattern': ->(s, settings) { settings.add_patterns(s, settings.out_archive_file_patterns) },
        'out-dirpattern': ->(s, settings) { settings.add_patterns(s, settings.out_dir_patterns) },
        'out-ext': ->(s, settings) { settings.add_exts(s, settings.out_extensions) },
        'out-filepattern': ->(s, settings) { settings.add_patterns(s, settings.out_file_patterns) },
        'out-filetype': ->(s, settings) { settings.add_file_types(s, settings.out_file_types) },
        'out-linesafterpattern': ->(s, settings) { settings.add_patterns(s, settings.out_lines_after_patterns) },
        'out-linesbeforepattern': ->(s, settings) { settings.add_patterns(s, settings.out_lines_before_patterns) },
        path: ->(s, settings) { settings.add_path(s) },
        searchpattern: ->(s, settings) { settings.add_patterns(s, settings.search_patterns) },
        'settings-file': ->(s, settings) { settings_from_file(s, settings) },
        'sort-by': ->(s, settings) { settings.set_sort_by_for_name(s) }
      }
      @int_action_dict = {
        linesafter: ->(i, settings) { settings.lines_after = i },
        linesbefore: ->(i, settings) { settings.lines_before = i },
        maxdepth: ->(i, settings) { settings.max_depth = i },
        maxlinelength: ->(i, settings) { settings.max_line_length = i },
        maxsize: ->(i, settings) { settings.max_size = i },
        mindepth: ->(i, settings) { settings.min_depth = i },
        minsize: ->(i, settings) { settings.min_size = i },
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
        @options.push(SearchOption.new(short, long, desc))
        @long_arg_dict[long] = long_sym
        @long_arg_dict[short] = long_sym if short
      end
    rescue StandardError => e
      raise SearchError, "#{e} (file: #{search_options_json_path})"
    ensure
      f&.close
    end
  end
end
