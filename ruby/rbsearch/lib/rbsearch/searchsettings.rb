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
  end
end
