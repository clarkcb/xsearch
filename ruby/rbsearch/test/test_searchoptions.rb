################################################################################
#
# searchoptions_test.rb
#
# Test the SearchOptions class
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'
 
class SearchOptionsTest < Minitest::Test
  def setup
    @searchoptions = SearchOptions.new
  end

  def test_no_args
    settings = @searchoptions.search_settings_from_args([])
    assert_equal(settings.archivesonly, false)
    assert_equal(settings.debug, false)
    assert_equal(settings.excludehidden, true)
    assert_equal(settings.firstmatch, false)
    assert_equal(settings.linesafter, 0)
    assert_equal(settings.linesbefore, 0)
    assert_equal(settings.listdirs, false)
    assert_equal(settings.listfiles, false)
    assert_equal(settings.listlines, false)
    assert_equal(settings.maxlinelength, 150)
    assert_equal(settings.multilinesearch, false)
    assert_equal(settings.printresults, true)
    assert_equal(settings.printusage, false)
    assert_equal(settings.printversion, false)
    assert_equal(settings.recursive, true)
    assert_equal(settings.searcharchives, false)
    assert_equal(settings.startpath, nil)
    assert_equal(settings.uniquelines, false)
    assert_equal(settings.verbose, false)
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
    assert(settings.searchpatterns.empty?)
  end

  def test_valid_args
    args = %w[-x py,rb -s Search .]
    settings = @searchoptions.search_settings_from_args(args)
    assert_equal(settings.startpath, '.')
    assert_equal(settings.in_extensions.length, 2)
    assert(settings.in_extensions.include?('py'))
    assert(settings.in_extensions.include?('rb'))
    assert_equal(settings.searchpatterns.length, 1)
    assert_equal(settings.searchpatterns.first.source, 'Search')
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
    json = <<-JSON
{
  "startpath": "~/src/xsearch/",
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