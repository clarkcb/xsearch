# SearchOptions - generate help, create settings from CLI args
require 'json'

require 'rbfind/arg_tokenizer'
require 'rbfind/common'
require_relative 'searcherror'
require_relative 'searchoption'
require_relative 'searchsettings'

module RbSearch

  # SearchOptions - parses CLI args into settings, generates usage string
  class SearchOptions
    attr_reader :options

    def initialize
      @options = []
      @bool_action_dict = {}
      @str_action_dict = {}
      @int_action_dict = {}
      set_actions
      set_options_from_json
      @options.sort! { |a, b| a.sort_arg <=> b.sort_arg }
      @arg_tokenizer = RbFind::ArgTokenizer.new(@options)
    end

    def update_settings_from_json(settings, json)
      arg_tokens = @arg_tokenizer.tokenize_json(json)
      update_settings_from_arg_tokens(settings, arg_tokens)
    end

    def update_settings_from_file(settings, file_path)
      arg_tokens = @arg_tokenizer.tokenize_file(file_path)
      update_settings_from_arg_tokens(settings, arg_tokens)
    end

    def update_settings_from_args(settings, args)
      arg_tokens = @arg_tokenizer.tokenize_args(args)
      update_settings_from_arg_tokens(settings, arg_tokens)
    end

    def search_settings_from_args(args)
      settings = SearchSettings.new
      # default print_results to true since running as cli
      settings.print_results = true
      update_settings_from_args(settings, args)
      settings
    end

    def usage
      puts "#{get_usage_string}\n"
      abort
    end

    private

    def get_usage_string
      usage = ["Usage:\n", " rbsearch [options] -s <searchpattern> <path> [<path> ...]\n\n", "Options:\n"]
      opt_strings = []
      opt_descs = []
      longest = 0
      @options.each do |opt|
        if opt.short_arg.empty?
          opt_string = "--#{opt.long_arg}"
        else
          opt_string = "-#{opt.short_arg},--#{opt.long_arg}"
        end
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
      usage.join
    end

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
        'settings-file': ->(s, settings) { update_settings_from_file(settings, s) },
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
        arg_type = RbFind::ArgTokenType::UNKNOWN
        long_sym = long.to_sym
        if @bool_action_dict.key?(long_sym)
          arg_type = RbFind::ArgTokenType::BOOL
        elsif @str_action_dict.key?(long_sym)
          arg_type = RbFind::ArgTokenType::STR
        elsif @int_action_dict.key?(long_sym)
          arg_type = RbFind::ArgTokenType::INT
        end
        @options.push(SearchOption.new(short, long, desc, arg_type))
      end
    rescue StandardError => e
      raise SearchError, "#{e} (file: #{search_options_json_path})"
    ensure
      f&.close
    end

    def update_settings_from_arg_tokens(settings, arg_tokens)
      arg_tokens.each do |arg_token|
        if arg_token.type == RbFind::ArgTokenType::BOOL
          if arg_token.value == true || arg_token.value == false
            @bool_action_dict[arg_token.name].call(arg_token.value, settings)
            return if [:help, :version].include?(arg_token.name)
          else
            raise SearchError, "Invalid value for option: #{arg_token.name}"
          end
        elsif arg_token.type == RbFind::ArgTokenType::STR
          if arg_token.value.is_a?(String)
            @str_action_dict[arg_token.name].call(arg_token.value, settings)
          elsif arg_token.value.is_a?(Array)
            arg_token.value.each do |v|
              if v.is_a?(String)
                @str_action_dict[arg_token.name].call(v, settings)
              else
                raise SearchError, "Invalid value for option: #{arg_token.name}"
              end
            end
          else
            raise SearchError, "Invalid value for option: #{arg_token.name}"
          end
        elsif arg_token.type == RbFind::ArgTokenType::INT
          if arg_token.value.is_a?(Numeric)
            @int_action_dict[arg_token.name].call(arg_token.value, settings)
          else
            raise SearchError, "Invalid value for option: #{arg_token.name}"
          end
        else
          raise SearchError, "Invalid option: #{arg_token.name}"
        end
      end
    end
  end
end
