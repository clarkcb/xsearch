################################################################################
#
# searchoptions_test.rb
#
# Test the SearchOptions class
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'

module RbSearch

  class SearchOptionsTest < Minitest::Test
    def setup
      @searchoptions = RbSearch::SearchOptions.new
    end

    def test_no_args
      settings = @searchoptions.search_settings_from_args([])
      assert_equal(false, settings.archivesonly)
      assert_equal(false, settings.debug)
      assert_equal(true, settings.excludehidden)
      assert_equal(false, settings.firstmatch)
      assert_equal(0, settings.linesafter)
      assert_equal(0, settings.linesbefore)
      assert_equal(false, settings.listdirs)
      assert_equal(false, settings.listfiles)
      assert_equal(false, settings.listlines)
      assert_equal(150, settings.maxlinelength)
      assert_equal(false, settings.multilinesearch)
      assert_equal(true, settings.printresults)
      assert_equal(false, settings.printusage)
      assert_equal(false, settings.printversion)
      assert_equal(true, settings.recursive)
      assert_equal(false, settings.searcharchives)
      assert_equal(false, settings.uniquelines)
      assert_equal(false, settings.verbose)
      assert(settings.in_archiveextensions.empty?)
      assert(settings.in_archivefilepatterns.empty?)
      assert(settings.in_dirpatterns.empty?)
      assert(settings.in_filepatterns.empty?)
      assert(settings.in_linesafterpatterns.empty?)
      assert(settings.in_linesbeforepatterns.empty?)
      assert(settings.linesaftertopatterns.empty?)
      assert(settings.linesafteruntilpatterns.empty?)
      assert(settings.out_archiveextensions.empty?)
      assert(settings.out_archivefilepatterns.empty?)
      assert(settings.out_dirpatterns.empty?)
      assert(settings.out_filepatterns.empty?)
      assert(settings.out_linesafterpatterns.empty?)
      assert(settings.out_linesbeforepatterns.empty?)
      assert(settings.paths.empty?)
      assert(settings.searchpatterns.empty?)
    end

    def test_valid_args
      args = %w[-x py,rb -s Search .]
      settings = @searchoptions.search_settings_from_args(args)
      assert_equal(2, settings.in_extensions.length)
      assert(settings.in_extensions.include?('py'))
      assert(settings.in_extensions.include?('rb'))
      assert_equal(1, settings.paths.length)
      assert(settings.paths.include?('.'))
      assert_equal(1, settings.searchpatterns.length)
      assert_equal('Search', settings.searchpatterns.first.source)
    end

    def test_archivesonly_arg
      args = ['--archivesonly']
      settings = @searchoptions.search_settings_from_args(args)
      assert(settings.archivesonly)
      assert(settings.searcharchives)
    end

    def test_debug_arg
      args = ['--debug']
      settings = @searchoptions.search_settings_from_args(args)
      assert(settings.debug)
      assert(settings.verbose)
    end

    def test_missing_arg
      args = %w[-x py,rb -s Search . -D]
      assert_raises(SearchError) { _settings = @searchoptions.search_settings_from_args(args) }
    end

    def test_invalid_arg
      args = %w[-x py,rb -s Search . -Q]
      assert_raises(SearchError) { _settings = @searchoptions.search_settings_from_args(args) }
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
        "includehidden": true
      }
      JSON
      @searchoptions.settings_from_json(json, settings)
      assert(settings.debug)
      assert(settings.verbose)
    end
  end
end
