################################################################################
#
# file_types_test.rb
#
# Test the FileTypes class
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'

module RbSearch
  class SearchSettingsTest < Test::Unit::TestCase
    def setup
      @settings = SearchSettings.new
    end

    def test_default_settings
      assert_equal(false, @settings.archives_only)
      assert_equal(false, @settings.debug)
      assert_equal(false, @settings.first_match)
      assert_equal(false, @settings.follow_symlinks)
      assert_equal(false, @settings.include_hidden)
      assert_equal(0, @settings.lines_after)
      assert_equal(0, @settings.lines_before)
      assert_equal(150, @settings.max_line_length)
      assert_equal(false, @settings.multi_line_search)
      assert_equal(false, @settings.print_dirs)
      assert_equal(false, @settings.print_files)
      assert_equal(false, @settings.print_lines)
      assert_equal(true, @settings.print_results)
      assert_equal(false, @settings.print_usage)
      assert_equal(false, @settings.print_version)
      assert_equal(true, @settings.recursive)
      assert_equal(false, @settings.search_archives)
      assert_equal(false, @settings.unique_lines)
      assert_equal(false, @settings.verbose)
      assert(@settings.in_archive_extensions.empty?)
      assert(@settings.in_archive_file_patterns.empty?)
      assert(@settings.in_dir_patterns.empty?)
      assert(@settings.in_file_patterns.empty?)
      assert(@settings.in_lines_after_patterns.empty?)
      assert(@settings.in_lines_before_patterns.empty?)
      assert(@settings.lines_after_to_patterns.empty?)
      assert(@settings.lines_after_until_patterns.empty?)
      assert(@settings.out_archive_extensions.empty?)
      assert(@settings.out_archive_file_patterns.empty?)
      assert(@settings.out_dir_patterns.empty?)
      assert(@settings.out_file_patterns.empty?)
      assert(@settings.out_lines_after_patterns.empty?)
      assert(@settings.out_lines_before_patterns.empty?)
      assert(@settings.paths.empty?)
      assert(@settings.search_patterns.empty?)
    end

    def test_set_properties
      @settings.archives_only = true
      @settings.debug = true
      @settings.lines_after = 5
      @settings.lines_before = 5
      assert_equal(true, @settings.archives_only)
      assert_equal(true, @settings.search_archives)
      assert_equal(true, @settings.debug)
      assert_equal(true, @settings.verbose)
      assert_equal(5, @settings.lines_after)
      assert_equal(5, @settings.lines_before)
    end

    def test_add_single_extension
      @settings.add_exts('rb', @settings.in_extensions)
      self.assert_equal(1, @settings.in_extensions.length)
      self.assert(@settings.in_extensions.include?('rb'))
    end

    def test_add_comma_delimited_extensions
      @settings.add_exts('py,rb', @settings.in_extensions)
      assert_equal(2, @settings.in_extensions.length)
      assert(@settings.in_extensions.include?('py'))
      assert(@settings.in_extensions.include?('rb'))
    end

    def test_add_extensions_as_array
      @settings.add_exts(%w[py rb], @settings.in_extensions)
      assert_equal(2, @settings.in_extensions.length)
      assert(@settings.in_extensions.include?('py'))
      assert(@settings.in_extensions.include?('rb'))
    end

    def test_add_pattern
      @settings.add_pattern('Search', @settings.search_patterns)
      assert_equal(1, @settings.search_patterns.length)
      assert_equal(@settings.search_patterns.first.source, 'Search')
    end

    def test_add_patterns_as_array
      @settings.add_patterns(%w[Search FileTypes], @settings.search_patterns)
      assert_equal(2, @settings.search_patterns.length)
      assert(@settings.search_patterns.any? {|p| p.source == 'Search'})
      assert(@settings.search_patterns.any? {|p| p.source == 'FileTypes'})
    end
  end
end
