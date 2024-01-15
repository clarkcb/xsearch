require 'rbfind/filetypes'
require 'rbfind/findsettings'

module RbSearch

  # SearchSettings - encapsulates search settings
  class SearchSettings < RbFind::FindSettings
    attr_accessor :colorize
    attr_accessor :first_match
    attr_accessor :in_lines_after_patterns
    attr_accessor :in_lines_before_patterns
    attr_accessor :lines_after
    attr_accessor :lines_after_to_patterns
    attr_accessor :lines_after_until_patterns
    attr_accessor :lines_before
    attr_accessor :list_lines
    attr_accessor :max_line_length
    attr_accessor :multi_line_search
    attr_accessor :out_lines_after_patterns
    attr_accessor :out_lines_before_patterns
    attr_accessor :print_results
    attr_accessor :search_archives
    attr_accessor :search_patterns
    attr_accessor :text_file_encoding
    attr_accessor :unique_lines

    def initialize
      super
      @colorize = true
      @first_match = false
      @lines_after = 0
      @lines_before = 0
      @list_lines = false
      @max_line_length = 150
      @multi_line_search = false
      @print_results = true
      @search_archives = false
      @text_file_encoding = 'utf-8'
      @unique_lines = false

      @in_lines_after_patterns = []
      @in_lines_before_patterns = []
      @lines_after_to_patterns = []
      @lines_after_until_patterns = []
      @out_lines_after_patterns = []
      @out_lines_before_patterns = []
      @search_patterns = []
    end

    def archives_only=(bool)
      @archives_only = bool
      @search_archives = bool if bool
      @include_archives = bool if bool
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
      s << ', ' + list_to_s('in_archive_extensions', @in_archive_extensions)
      s << ', ' + list_to_s('in_archive_file_patterns', @in_archive_file_patterns)
      s << ", include_hidden: #{@include_hidden}"
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
      s << ", max_depth: #{@max_depth}"
      s << ", max_last_mod: #{@max_last_mod}"
      s << ", max_line_length: #{@max_line_length}"
      s << ", max_size: #{@max_size}"
      s << ", min_depth: #{@min_depth}"
      s << ", min_last_mod: #{@min_last_mod}"
      s << ", min_size: #{@min_size}"
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
      s << ", sort_by: #{@sort_by}"
      s << ", sort_case_insensitive: #{@sort_case_insensitive}"
      s << ", sort_descending: #{@sort_descending}"
      s << ", text_file_encoding: \"#{@text_file_encoding}\""
      s << ", unique_lines: #{@unique_lines}"
      s << ", verbose: #{@verbose}"
      s << ')'
      s
    end
  end
end
