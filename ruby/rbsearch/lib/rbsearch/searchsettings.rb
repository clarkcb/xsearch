require 'rbfind/color'
require 'rbfind/filetypes'
require 'rbfind/findsettings'

module RbSearch

  # SearchSettings - encapsulates search settings
  class SearchSettings < RbFind::FindSettings
    attr_accessor :first_match,
                  :in_lines_after_patterns,
                  :in_lines_before_patterns,
                  :line_color,
                  :lines_after,
                  :lines_after_to_patterns,
                  :lines_after_until_patterns,
                  :lines_before,
                  :max_line_length,
                  :multi_line_search,
                  :out_lines_after_patterns,
                  :out_lines_before_patterns,
                  :print_lines,
                  :print_matches,
                  :print_results,
                  :search_archives,
                  :search_patterns,
                  :text_file_encoding,
                  :unique_lines

    def initialize
      super
      @first_match = false
      @in_lines_after_patterns = Set.new
      @in_lines_before_patterns = Set.new
      @line_color = RbFind::Color::GREEN
      @lines_after = 0
      @lines_after_to_patterns = Set.new
      @lines_after_until_patterns = Set.new
      @lines_before = 0
      @max_line_length = 150
      @multi_line_search = false
      @out_lines_after_patterns = Set.new
      @out_lines_before_patterns = Set.new
      @print_lines = false
      @print_matches = false
      @print_results = true
      @search_archives = false
      @search_patterns = Set.new
      @text_file_encoding = 'utf-8'
      @unique_lines = false
    end

    def archives_only=(archives_only)
      @archives_only = archives_only
      @include_archives = archives_only if archives_only
      @search_archives = archives_only if archives_only
    end

    def to_s
      'SearchSettings(' +
        "archives_only: #{@archives_only}" +
        ", colorize: #{@colorize}" +
        ", debug: #{@debug}" +
        ", first_match: #{@first_match}" +
        ", follow_symlinks: #{@follow_symlinks}" +
        ', ' + set_to_s('in_archive_extensions', @in_archive_extensions) +
        ', ' + set_to_s('in_archive_file_patterns', @in_archive_file_patterns) +
        ", include_hidden: #{@include_hidden}" +
        ', ' + array_to_s('in_dir_patterns', @in_dir_patterns.map { |p| p.source }) +
        ', ' + set_to_s('in_extensions', @in_extensions) +
        ', ' + array_to_s('in_file_patterns', @in_file_patterns.map { |p| p.source }) +
        ', ' + file_types_to_s('in_file_types', @in_file_types) +
        ', ' + array_to_s('in_lines_after_patterns', @in_lines_after_patterns.map { |p| p.source }) +
        ', ' + array_to_s('in_lines_before_patterns', @in_lines_before_patterns.map { |p| p.source }) +
        ", lines_after: #{@lines_after}" +
        ', ' + array_to_s('lines_after_to_patterns', @lines_after_to_patterns.map { |p| p.source }) +
        ', ' + array_to_s('lines_after_until_patterns', @lines_after_until_patterns.map { |p| p.source }) +
        ", lines_before: #{@lines_before}" +
        ", max_depth: #{@max_depth}" +
        ", max_last_mod=#{last_mod_to_s(@max_last_mod)}" +
        ", max_line_length: #{@max_line_length}" +
        ", max_size: #{@max_size}" +
        ", min_depth: #{@min_depth}" +
        ", min_last_mod=#{last_mod_to_s(@min_last_mod)}" +
        ", min_size: #{@min_size}" +
        ", multi_line_search: #{@multi_line_search}" +
        ', ' + set_to_s('out_archive_extensions', @out_archive_extensions) +
        ', ' + array_to_s('out_archive_file_patterns', @out_archive_file_patterns.map { |p| p.source }) +
        ', ' + array_to_s('out_dir_patterns', @out_dir_patterns.map { |p| p.source }) +
        ', ' + set_to_s('out_extensions', @out_extensions) +
        ', ' + array_to_s('out_file_patterns', @out_file_patterns.map { |p| p.source }) +
        ', ' + file_types_to_s('out_file_types', @out_file_types) +
        ', ' + array_to_s('out_lines_after_patterns', @out_lines_after_patterns.map { |p| p.source }) +
        ', ' + array_to_s('out_lines_before_patterns', @out_lines_before_patterns.map { |p| p.source }) +
        ', ' + paths_to_s('paths', @paths) +
        ", print_dirs: #{@print_dirs}" +
        ", print_files: #{@print_files}" +
        ", print_lines: #{@print_lines}" +
        ", print_matches: #{@print_matches}" +
        ", print_results: #{@print_results}" +
        ", print_usage: #{@print_usage}" +
        ", print_version: #{@print_version}" +
        ", recursive: #{@recursive}" +
        ", search_archives: #{@search_archives}" +
        ', ' + array_to_s('search_patterns', @search_patterns.map { |p| p.source }) +
        ", sort_by: #{@sort_by}" +
        ", sort_case_insensitive: #{@sort_case_insensitive}" +
        ", sort_descending: #{@sort_descending}" +
        ", text_file_encoding: \"#{@text_file_encoding}\"" +
        ", unique_lines: #{@unique_lines}" +
        ", verbose: #{@verbose}" +
        ')'
    end
  end
end
