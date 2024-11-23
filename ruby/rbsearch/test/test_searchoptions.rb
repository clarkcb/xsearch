################################################################################
#
# searchoptions_test.rb
#
# Test the SearchOptions class
#
################################################################################

require_relative '../lib/rbsearch'
require 'test/unit'

module RbSearch

  class SearchOptionsTest < Test::Unit::TestCase
    def setup
      @search_options = RbSearch::SearchOptions.new
    end

    def test_no_args
      settings = @search_options.search_settings_from_args([])
      assert_equal(false, settings.archives_only)
      assert_equal(false, settings.debug)
      assert_equal(false, settings.first_match)
      assert_equal(false, settings.follow_symlinks)
      assert_equal(false, settings.include_hidden)
      assert_equal(0, settings.lines_after)
      assert_equal(0, settings.lines_before)
      assert_equal(150, settings.max_line_length)
      assert_equal(false, settings.multi_line_search)
      assert_equal(false, settings.print_dirs)
      assert_equal(false, settings.print_files)
      assert_equal(false, settings.print_lines)
      assert_equal(true, settings.print_results)
      assert_equal(false, settings.print_usage)
      assert_equal(false, settings.print_version)
      assert_equal(true, settings.recursive)
      assert_equal(false, settings.search_archives)
      assert_equal(false, settings.unique_lines)
      assert_equal(false, settings.verbose)
      assert(settings.in_archive_extensions.empty?)
      assert(settings.in_archive_file_patterns.empty?)
      assert(settings.in_dir_patterns.empty?)
      assert(settings.in_file_patterns.empty?)
      assert(settings.in_lines_after_patterns.empty?)
      assert(settings.in_lines_before_patterns.empty?)
      assert(settings.lines_after_to_patterns.empty?)
      assert(settings.lines_after_until_patterns.empty?)
      assert(settings.out_archive_extensions.empty?)
      assert(settings.out_archive_file_patterns.empty?)
      assert(settings.out_dir_patterns.empty?)
      assert(settings.out_file_patterns.empty?)
      assert(settings.out_lines_after_patterns.empty?)
      assert(settings.out_lines_before_patterns.empty?)
      assert(settings.paths.empty?)
      assert(settings.search_patterns.empty?)
    end

    def test_valid_args
      args = %w[-x py,rb -s Search .]
      settings = @search_options.search_settings_from_args(args)
      assert_equal(2, settings.in_extensions.length)
      assert(settings.in_extensions.include?('py'))
      assert(settings.in_extensions.include?('rb'))
      assert_equal(1, settings.paths.length)
      assert(settings.paths.include?('.'))
      assert_equal(1, settings.search_patterns.length)
      assert_equal('Search', settings.search_patterns.first.source)
    end

    def test_archives_only_arg
      args = ['--archivesonly']
      settings = @search_options.search_settings_from_args(args)
      assert(settings.archives_only)
      assert(settings.search_archives)
    end

    def test_debug_arg
      args = ['--debug']
      settings = @search_options.search_settings_from_args(args)
      assert(settings.debug)
      assert(settings.verbose)
    end

    def test_missing_arg
      args = %w[-x py,rb -s Search . -D]
      assert_raises(SearchError) { _settings = @search_options.search_settings_from_args(args) }
    end

    def test_invalid_arg
      args = %w[-x py,rb -s Search . -Q]
      assert_raises(SearchError) { _settings = @search_options.search_settings_from_args(args) }
    end

    def test_settings_from_json
      settings = SearchSettings.new
      json = <<~JSON
      {
        "path": "~/src/xsearch/",
        "in-ext": ["js","ts"],
        "out-dirpattern": "node_module",
        "out-filepattern": ["temp"],
        "searchpattern": "Searcher",
        "linesbefore": 2,
        "linesafter": 2,
        "debug": true,
        "allmatches": false,
        "followsymlinks": true,
        "includehidden": true
      }
      JSON
      @search_options.settings_from_json(json, settings)
      assert(settings.debug)
      assert(settings.verbose)
      assert(settings.follow_symlinks)
      assert(settings.include_hidden)
    end
  end
end
