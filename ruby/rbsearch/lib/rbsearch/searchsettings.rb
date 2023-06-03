require_relative 'filetypes'

module RbSearch

  # SearchSettings - encapsulates search settings
  class SearchSettings

    attr_reader :archives_only
    attr_accessor :colorize
    attr_reader :debug
    attr_accessor :exclude_hidden
    attr_accessor :first_match
    attr_accessor :in_archive_extensions
    attr_accessor :in_archive_file_patterns
    attr_accessor :in_dir_patterns
    attr_accessor :in_extensions
    attr_accessor :in_file_patterns
    attr_accessor :in_file_types
    attr_accessor :in_lines_after_patterns
    attr_accessor :in_lines_before_patterns
    attr_accessor :lines_after
    attr_accessor :lines_after_to_patterns
    attr_accessor :lines_after_until_patterns
    attr_accessor :lines_before
    attr_accessor :list_dirs
    attr_accessor :list_files
    attr_accessor :list_lines
    attr_accessor :max_line_length
    attr_accessor :multi_line_search
    attr_accessor :out_archive_extensions
    attr_accessor :out_archive_file_patterns
    attr_accessor :out_dir_patterns
    attr_accessor :out_extensions
    attr_accessor :out_file_patterns
    attr_accessor :out_file_types
    attr_accessor :out_lines_after_patterns
    attr_accessor :out_lines_before_patterns
    attr_accessor :paths
    attr_accessor :print_results
    attr_accessor :print_usage
    attr_accessor :print_version
    attr_accessor :recursive
    attr_accessor :search_archives
    attr_accessor :search_patterns
    attr_accessor :text_file_encoding
    attr_accessor :unique_lines
    attr_accessor :verbose

    def initialize
      @archives_only = false
      @colorize = true
      @debug = false
      @exclude_hidden = true
      @first_match = false
      @lines_after = 0
      @lines_before = 0
      @list_dirs = false
      @list_files = false
      @list_lines = false
      @max_line_length = 150
      @multi_line_search = false
      @print_results = true
      @print_usage = false
      @print_version = false
      @recursive = true
      @search_archives = false
      @text_file_encoding = 'utf-8'
      @unique_lines = false
      @verbose = false

      @in_archive_extensions = []
      @in_archive_file_patterns = []
      @in_dir_patterns = []
      @in_extensions = []
      @in_file_patterns = []
      @in_file_types = []
      @in_lines_after_patterns = []
      @in_lines_before_patterns = []
      @lines_after_to_patterns = []
      @lines_after_until_patterns = []
      @out_archive_extensions = []
      @out_archive_file_patterns = []
      @out_dir_patterns = []
      @out_extensions = []
      @out_file_patterns = []
      @out_file_types = []
      @out_lines_after_patterns = []
      @out_lines_before_patterns = []
      @paths = []
      @search_patterns = []
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

    def add_file_types(file_types, file_types_set)
      if file_types.instance_of? String
        file_types.split(',').each do |t|
          file_types_set.push(FileTypes.from_name(t))
        end
      elsif file_types.instance_of? Array
        file_types.each do |t|
          file_types_set.push(FileTypes.from_name(t))
        end
      end
    end

    def archives_only=(bool)
      @archives_only = bool
      @search_archives = bool if bool
    end

    def debug=(bool)
      @debug = bool
      @verbose = bool if bool
    end

    def to_s
      s = 'SearchSettings('
      s << "archives_only: #{@archives_only}"
      s << ", debug: #{@debug}"
      s << ", first_match: #{@first_match}"
      s << ", exclude_hidden: #{@exclude_hidden}"
      s << ', ' + list_to_s('in_archive_extensions', @in_archive_extensions)
      s << ', ' + list_to_s('in_archive_file_patterns', @in_archive_file_patterns)
      s << ', ' + list_to_s('in_dir_patterns', @in_dir_patterns)
      s << ', ' + list_to_s('in_extensions', @in_extensions)
      s << ', ' + list_to_s('in_file_patterns', @in_file_patterns)
      s << ', ' + file_types_to_s('in_file_types', @in_file_types)
      s << ', ' + list_to_s('in_lines_after_patterns', @in_lines_after_patterns)
      s << ', ' + list_to_s('in_lines_before_patterns', @in_lines_before_patterns)
      s << ", lines_after: #{@lines_after}"
      s << ', ' + list_to_s('lines_after_to_patterns', @lines_after_to_patterns)
      s << ', ' + list_to_s('lines_after_until_patterns', @lines_after_until_patterns)
      s << ", lines_before: #{@lines_before}"
      s << ", list_dirs: #{@list_dirs}"
      s << ", list_files: #{@list_files}"
      s << ", list_lines: #{@list_lines}"
      s << ", max_line_length: #{@max_line_length}"
      s << ", multi_line_search: #{@multi_line_search}"
      s << ', ' + list_to_s('out_archive_extensions', @out_archive_extensions)
      s << ', ' + list_to_s('out_archive_file_patterns', @out_archive_file_patterns)
      s << ', ' + list_to_s('out_dir_patterns', @out_dir_patterns)
      s << ', ' + list_to_s('out_extensions', @out_extensions)
      s << ', ' + list_to_s('out_file_patterns', @out_file_patterns)
      s << ', ' + file_types_to_s('out_file_types', @out_file_types)
      s << ', ' + list_to_s('out_lines_after_patterns', @out_lines_after_patterns)
      s << ', ' + list_to_s('out_lines_before_patterns', @out_lines_before_patterns)
      s << ', ' + list_to_s('paths', @paths)
      s << ", print_results: #{@print_results}"
      s << ", print_usage: #{@print_usage}"
      s << ", print_version: #{@print_version}"
      s << ", recursive: #{@recursive}"
      s << ", search_archives: #{@search_archives}"
      s << ', ' + list_to_s('search_patterns', @search_patterns)
      s << ", text_file_encoding: \"#{@text_file_encoding}\""
      s << ", unique_lines: #{@unique_lines}"
      s << ", verbose: #{@verbose}"
      s << ')'
      s
    end

    private

    def list_to_s(name, lst)
      "#{name}=[\"#{lst.join('", "')}\"]"
    end

    def file_types_to_s(name, file_types)
      s = "#{name}=["
      count = 0
      file_types.each do |ft|
        s << ', ' if count.positive?
        s << "\"#{FileTypes.to_name(ft)}\""
        count += 1
      end
      s + ']'
    end
  end
end
