################################################################################
#
# filetypes_test.rb
#
# Test the FileTypes class
#
################################################################################

require_relative "../rbsearch/searchsettings.rb"
require "test/unit"
 
class SearchSettingsTest < Test::Unit::TestCase
  def setup
    @settings = SearchSettings.new
  end

  def test_default_settings
    assert_equal(@settings.archivesonly, false)
    assert_equal(@settings.debug, false)
    assert_equal(@settings.excludehidden, true)
    assert_equal(@settings.firstmatch, false)
    assert_equal(@settings.linesafter, 0)
    assert_equal(@settings.linesbefore, 0)
    assert_equal(@settings.listdirs, false)
    assert_equal(@settings.listfiles, false)
    assert_equal(@settings.listlines, false)
    assert_equal(@settings.maxlinelength, 150)
    assert_equal(@settings.multilinesearch, false)
    assert_equal(@settings.printresults, true)
    assert_equal(@settings.printusage, false)
    assert_equal(@settings.printversion, false)
    assert_equal(@settings.recursive, true)
    assert_equal(@settings.searcharchives, false)
    assert_equal(@settings.startpath, nil)
    assert_equal(@settings.uniquelines, false)
    assert_equal(@settings.verbose, false)
    assert(@settings.in_archiveextensions.empty?)
    assert(@settings.in_archivefilepatterns.empty?)
    assert(@settings.in_dirpatterns.empty?)
    assert(@settings.in_filepatterns.empty?)
    assert(@settings.in_linesafterpatterns.empty?)
    assert(@settings.in_linesbeforepatterns.empty?)
    assert(@settings.linesaftertopatterns.empty?)
    assert(@settings.linesafteruntilpatterns.empty?)
    assert(@settings.out_archiveextensions.empty?)
    assert(@settings.out_archivefilepatterns.empty?)
    assert(@settings.out_dirpatterns.empty?)
    assert(@settings.out_filepatterns.empty?)
    assert(@settings.out_linesafterpatterns.empty?)
    assert(@settings.out_linesbeforepatterns.empty?)
    assert(@settings.searchpatterns.empty?)
  end

  def test_set_properties
    @settings.archivesonly = true
    @settings.debug = true
    @settings.linesafter = 5
    @settings.linesbefore = 5
    assert_equal(@settings.archivesonly, true)
    assert_equal(@settings.debug, true)
    assert_equal(@settings.linesafter, 5)
    assert_equal(@settings.linesbefore, 5)
  end

  def test_add_single_extension
    @settings.add_exts('rb', @settings.in_extensions)
    self.assert_equal(@settings.in_extensions.length, 1)
    self.assert(@settings.in_extensions.include?('rb'))
  end

  def test_add_comma_delimited_extensions
    @settings.add_exts('py,rb', @settings.in_extensions)
    assert_equal(@settings.in_extensions.length, 2)
    assert(@settings.in_extensions.include?('py'))
    assert(@settings.in_extensions.include?('rb'))
  end

  def test_add_pattern
    @settings.add_pattern('Search', @settings.searchpatterns)
    assert_equal(@settings.searchpatterns.length, 1)
    assert_equal(@settings.searchpatterns.first.source, 'Search')
  end
end
