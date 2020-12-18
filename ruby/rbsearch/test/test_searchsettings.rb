################################################################################
#
# filetypes_test.rb
#
# Test the FileTypes class
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'

module RbSearch

  class SearchSettingsTest < Minitest::Test
    def setup
      @settings = SearchSettings.new
    end

    def test_default_settings
      assert_equal(false, @settings.archivesonly)
      assert_equal(false, @settings.debug)
      assert_equal(true, @settings.excludehidden)
      assert_equal(false, @settings.firstmatch)
      assert_equal(0, @settings.linesafter)
      assert_equal(0, @settings.linesbefore)
      assert_equal(false, @settings.listdirs)
      assert_equal(false, @settings.listfiles)
      assert_equal(false, @settings.listlines)
      assert_equal(150, @settings.maxlinelength)
      assert_equal(false, @settings.multilinesearch)
      assert_equal(true, @settings.printresults)
      assert_equal(false, @settings.printusage)
      assert_equal(false, @settings.printversion)
      assert_equal(true, @settings.recursive)
      assert_equal(false, @settings.searcharchives)
      assert_equal(nil, @settings.startpath)
      assert_equal(false, @settings.uniquelines)
      assert_equal(false, @settings.verbose)
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
      assert_equal(true, @settings.archivesonly)
      assert_equal(true, @settings.searcharchives)
      assert_equal(true, @settings.debug)
      assert_equal(true, @settings.verbose)
      assert_equal(5, @settings.linesafter)
      assert_equal(5, @settings.linesbefore)
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
      @settings.add_pattern('Search', @settings.searchpatterns)
      assert_equal(1, @settings.searchpatterns.length)
      assert_equal(@settings.searchpatterns.first.source, 'Search')
    end

    def test_add_patterns_as_array
      @settings.add_patterns(%w[Search FileTypes], @settings.searchpatterns)
      assert_equal(2, @settings.searchpatterns.length)
      assert_equal(@settings.searchpatterns.first.source, 'Search')
      assert_equal(@settings.searchpatterns[1].source, 'FileTypes')
    end
  end
end
