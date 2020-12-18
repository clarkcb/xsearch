################################################################################
#
# searcher_test.rb
#
# Searcher testing
#
################################################################################

require_relative '../lib/rbsearch'
require 'minitest/autorun'

module RbSearch

  class SearcherTest < Minitest::Test

    def get_settings
      settings = SearchSettings.new
      settings.startpath = '.'
      settings.add_pattern('Searcher', settings.searchpatterns)
      settings
    end

    def get_test_file
      # File.expand_path("#{SHAREDPATH}/testFiles/testFile2.txt")
      File.join(File.dirname(__FILE__), "fixtures/testFile2.txt")
    end

    ################################################################################
    # is_search_dir tests
    ################################################################################
    def test_is_search_dir_no_patterns
      settings = get_settings
      searcher = Searcher.new(settings)
      dir = 'plsearch'
      assert(searcher.search_dir?(dir))
    end

    def test_is_search_dir_matches_in_pattern
      settings = get_settings
      settings.add_pattern('plsearch', settings.in_dirpatterns)
      searcher = Searcher.new(settings)
      dir = 'plsearch'
      assert(searcher.search_dir?(dir))
    end

    def test_is_search_dir_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('plsearch', settings.in_dirpatterns)
      searcher = Searcher.new(settings)
      dir = 'pysearch'
      assert(!searcher.search_dir?(dir))
    end

    def test_is_search_dir_matches_out_pattern
      settings = get_settings
      settings.add_pattern('pysearch', settings.out_dirpatterns)
      searcher = Searcher.new(settings)
      dir = 'pysearch'
      assert(!searcher.search_dir?(dir))
    end

    def test_is_search_dir_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('pysearch', settings.out_dirpatterns)
      searcher = Searcher.new(settings)
      dir = 'plsearch'
      assert(searcher.search_dir?(dir))
    end

    def test_is_search_dir_single_dot
      settings = get_settings
      searcher = Searcher.new(settings)
      dir = '.'
      assert(searcher.search_dir?(dir))
    end

    def test_is_search_dir_double_dot
      settings = get_settings
      searcher = Searcher.new(settings)
      dir = '..'
      assert(searcher.search_dir?(dir))
    end

    def test_is_search_dir_hidden_dir
      settings = get_settings
      searcher = Searcher.new(settings)
      dir = '.git'
      assert(!searcher.search_dir?(dir))
    end

    def test_is_search_dir_hidden_dir_include_hidden
      settings = get_settings
      settings.excludehidden = false
      searcher = Searcher.new(settings)
      dir = '.git'
      assert(searcher.search_dir?(dir))
    end

    ################################################################################
    # is_search_file tests
    ################################################################################
    def test_is_search_file_matches_by_default
      settings = get_settings
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(searcher.search_file?(f))
    end

    def test_is_search_file_matches_in_extension
      settings = get_settings
      settings.add_exts('rb', settings.in_extensions)
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(searcher.search_file?(f))
    end

    def test_is_search_file_no_match_in_extension
      settings = get_settings
      settings.add_exts('py', settings.in_extensions)
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(!searcher.search_file?(f))
    end

    def test_is_search_file_matches_out_extension
      settings = get_settings
      settings.add_exts('rb', settings.out_extensions)
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(!searcher.search_file?(f))
    end

    def test_is_search_file_no_match_out_extension
      settings = get_settings
      settings.add_exts('py', settings.out_extensions)
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(searcher.search_file?(f))
    end

    def test_is_search_file_matches_in_pattern
      settings = get_settings
      settings.add_pattern('search', settings.in_filepatterns)
      searcher = Searcher.new(settings)
      f = 'searcher.rb'
      assert(searcher.search_file?(f))
    end

    def test_is_search_file_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('search', settings.in_filepatterns)
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(!searcher.search_file?(f))
    end

    def test_is_search_file_matches_out_pattern
      settings = get_settings
      settings.add_pattern('search', settings.out_filepatterns)
      searcher = Searcher.new(settings)
      f = 'searcher.rb'
      assert(!searcher.search_file?(f))
    end

    def test_is_search_file_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('search', settings.out_filepatterns)
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(searcher.search_file?(f))
    end

    ################################################################################
    # is__archive_search_file tests
    ################################################################################
    def test_is_archive_search_file_matches_by_default
      settings = get_settings
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(searcher.archive_search_file?(f))
    end

    def test_is_archive_search_file_matches_in_extension
      settings = get_settings
      settings.add_exts('zip', settings.in_archiveextensions)
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(searcher.archive_search_file?(f))
    end

    def test_is_archive_search_file_no_match_in_extension
      settings = get_settings
      settings.add_exts('gz', settings.in_archiveextensions)
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(!searcher.archive_search_file?(f))
    end

    def test_is_archive_search_file_matches_out_extension
      settings = get_settings
      settings.add_exts('zip', settings.out_archiveextensions)
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(!searcher.archive_search_file?(f))
    end

    def test_is_archive_search_file_no_match_out_extension
      settings = get_settings
      settings.add_exts('gz', settings.out_archiveextensions)
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(searcher.archive_search_file?(f))
    end

    def test_is_archive_search_file_matches_in_pattern
      settings = get_settings
      settings.add_pattern('arch', settings.in_archivefilepatterns)
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(searcher.archive_search_file?(f))
    end

    def test_is_archive_search_file_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('archives', settings.in_archivefilepatterns)
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(!searcher.archive_search_file?(f))
    end

    def test_is_archive_search_file_matches_out_pattern
      settings = get_settings
      settings.add_pattern('arch', settings.out_archivefilepatterns)
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(!searcher.archive_search_file?(f))
    end

    def test_is_archive_search_file_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('archives', settings.out_archivefilepatterns)
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(searcher.archive_search_file?(f))
    end

    ################################################################################
    # filter_file tests
    ################################################################################
    def test_filter_file_matches_by_default
      settings = get_settings
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(searcher.filter_file?(f))
    end

    def test_filter_file_is_search_file
      settings = get_settings
      settings.add_exts('rb', settings.in_extensions)
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(searcher.filter_file?(f))
    end

    def test_filter_file_not_is_search_file
      settings = get_settings
      settings.add_exts('pl', settings.in_extensions)
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(!searcher.filter_file?(f))
    end

    def test_filter_file_is_hidden_file
      settings = get_settings
      searcher = Searcher.new(settings)
      f = '.gitignore'
      assert(!searcher.filter_file?(f))
    end

    def test_filter_file_hidden_includehidden
      settings = get_settings
      settings.excludehidden = false
      searcher = Searcher.new(settings)
      f = '.gitignore'
      assert(searcher.filter_file?(f))
    end

    def test_filter_file_archive_no_searcharchives
      settings = get_settings
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(!searcher.filter_file?(f))
    end

    def test_filter_file_archive_searcharchives
      settings = get_settings
      settings.searcharchives = 1
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(searcher.filter_file?(f))
    end

    def test_filter_file_archive_archivesonly
      settings = get_settings
      settings.archivesonly = true
      settings.searcharchives = true
      searcher = Searcher.new(settings)
      f = 'archive.zip'
      assert(searcher.filter_file?(f))
    end

    def test_filter_file_nonarchive_archivesonly
      settings = get_settings
      settings.archivesonly = true
      settings.searcharchives = true
      searcher = Searcher.new(settings)
      f = 'fileutil.rb'
      assert(!searcher.filter_file?(f))
    end

    ################################################################################
    # search_lines tests
    ################################################################################
    def test_search_lines
      settings = get_settings
      searcher = Searcher.new(settings)
      testfile = get_test_file
      fo = File.open(testfile, mode: 'r:ISO-8859-1')
      contents = fo.read
      results = searcher.search_multiline_string(contents)
      assert_equal(results.size, 2)

      first_result = results[0]
      assert_equal(first_result.linenum, 29)
      assert_equal(first_result.match_start_index, 3)
      assert_equal(first_result.match_end_index, 11)

      second_result = results[1]
      assert_equal(second_result.linenum, 35)
      assert_equal(second_result.match_start_index, 24)
      assert_equal(second_result.match_end_index, 32)
    ensure
      fo&.close
    end

    ################################################################################
    # search_multiline_string tests
    ################################################################################
    def test_search_multiline_string
      settings = get_settings
      searcher = Searcher.new(settings)
      testfile = get_test_file
      fo = File.open(testfile, mode: 'r:ISO-8859-1')
      line_iterator = fo.each_line
      results = searcher.search_line_iterator(line_iterator)
      assert_equal(results.size, 2)

      first_result = results[0]
      assert_equal(first_result.linenum, 29)
      assert_equal(first_result.match_start_index, 3)
      assert_equal(first_result.match_end_index, 11)

      second_result = results[1]
      assert_equal(second_result.linenum, 35)
      assert_equal(second_result.match_start_index, 24)
      assert_equal(second_result.match_end_index, 32)
    ensure
      fo&.close
    end
  end
end
