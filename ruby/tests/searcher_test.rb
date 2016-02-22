################################################################################
#
# searcher_test.rb
#
# Searcher testing
#
################################################################################

require_relative "../rbsearch/config.rb"
require_relative "../rbsearch/filetypes.rb"
require_relative "../rbsearch/searcher.rb"
require_relative "../rbsearch/searchsettings.rb"
require "test/unit"
 
class SearcherTest < Test::Unit::TestCase

  def get_settings
    settings = SearchSettings.new()
    settings.startpath = '.'
    settings.add_pattern('Searcher', settings.searchpatterns)
    settings
  end

  def get_test_file
    File.expand_path("#{SHAREDPATH}/testFiles/testFile2.txt")
  end

################################################################################
# is_search_dir tests
################################################################################
  def test_is_search_dir_no_patterns
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    dir = 'plsearch'
    assert(searcher.is_search_dir(dir))
  end

  def test_is_search_dir_matches_in_pattern
    settings = self.get_settings()
    settings.add_pattern('plsearch', settings.in_dirpatterns)
    searcher = Searcher.new(settings)
    dir = 'plsearch'
    assert(searcher.is_search_dir(dir))
  end

  def test_is_search_dir_no_match_in_pattern
    settings = self.get_settings()
    settings.add_pattern('plsearch', settings.in_dirpatterns)
    searcher = Searcher.new(settings)
    dir = 'pysearch'
    assert(!searcher.is_search_dir(dir))
  end

  def test_is_search_dir_matches_out_pattern
    settings = self.get_settings()
    settings.add_pattern('pysearch', settings.out_dirpatterns)
    searcher = Searcher.new(settings)
    dir = 'pysearch'
    assert(!searcher.is_search_dir(dir))
  end

  def test_is_search_dir_no_match_out_pattern
    settings = self.get_settings()
    settings.add_pattern('pysearch', settings.out_dirpatterns)
    searcher = Searcher.new(settings)
    dir = 'plsearch'
    assert(searcher.is_search_dir(dir))
  end

  def test_is_search_dir_single_dot
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    dir = '.'
    assert(searcher.is_search_dir(dir))
  end

  def test_is_search_dir_double_dot
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    dir = '..'
    assert(searcher.is_search_dir(dir))
  end

  def test_is_search_dir_hidden_dir
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    dir = '.git'
    assert(!searcher.is_search_dir(dir))
  end

  def test_is_search_dir_hidden_dir_include_hidden
    settings = self.get_settings()
    settings.excludehidden = false
    searcher = Searcher.new(settings)
    dir = '.git'
    assert(searcher.is_search_dir(dir))
  end

################################################################################
# is_search_file tests
################################################################################
  def test_is_search_file_matches_by_default
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(searcher.is_search_file(f))
  end

  def test_is_search_file_matches_in_extension
    settings = self.get_settings()
    settings.add_exts('rb', settings.in_extensions)
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(searcher.is_search_file(f))
  end

  def test_is_search_file_no_match_in_extension
    settings = self.get_settings()
    settings.add_exts('py', settings.in_extensions)
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(!searcher.is_search_file(f))
  end

  def test_is_search_file_matches_out_extension
    settings = self.get_settings()
    settings.add_exts('rb', settings.out_extensions)
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(!searcher.is_search_file(f))
  end

  def test_is_search_file_no_match_out_extension
    settings = self.get_settings()
    settings.add_exts('py', settings.out_extensions)
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(searcher.is_search_file(f))
  end

  def test_is_search_file_matches_in_pattern
    settings = self.get_settings()
    settings.add_pattern('search', settings.in_filepatterns)
    searcher = Searcher.new(settings)
    f = 'searcher.rb'
    assert(searcher.is_search_file(f))
  end

  def test_is_search_file_no_match_in_pattern
    settings = self.get_settings()
    settings.add_pattern('search', settings.in_filepatterns)
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(!searcher.is_search_file(f))
  end

  def test_is_search_file_matches_out_pattern
    settings = self.get_settings()
    settings.add_pattern('search', settings.out_filepatterns)
    searcher = Searcher.new(settings)
    f = 'searcher.rb'
    assert(!searcher.is_search_file(f))
  end

  def test_is_search_file_no_match_out_pattern
    settings = self.get_settings()
    settings.add_pattern('search', settings.out_filepatterns)
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(searcher.is_search_file(f))
  end

################################################################################
# is__archive_search_file tests
################################################################################
  def test_is_archive_search_file_matches_by_default
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(searcher.is_archive_search_file(f))
  end

  def test_is_archive_search_file_matches_in_extension
    settings = self.get_settings()
    settings.add_exts('zip', settings.in_archiveextensions)
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(searcher.is_archive_search_file(f))
  end

  def test_is_archive_search_file_no_match_in_extension
    settings = self.get_settings()
    settings.add_exts('gz', settings.in_archiveextensions)
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(!searcher.is_archive_search_file(f))
  end

  def test_is_archive_search_file_matches_out_extension
    settings = self.get_settings()
    settings.add_exts('zip', settings.out_archiveextensions)
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(!searcher.is_archive_search_file(f))
  end

  def test_is_archive_search_file_no_match_out_extension
    settings = self.get_settings()
    settings.add_exts('gz', settings.out_archiveextensions)
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(searcher.is_archive_search_file(f))
  end

  def test_is_archive_search_file_matches_in_pattern
    settings = self.get_settings()
    settings.add_pattern('arch', settings.in_archivefilepatterns)
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(searcher.is_archive_search_file(f))
  end

  def test_is_archive_search_file_no_match_in_pattern
    settings = self.get_settings()
    settings.add_pattern('archives', settings.in_archivefilepatterns)
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(!searcher.is_archive_search_file(f))
  end

  def test_is_archive_search_file_matches_out_pattern
    settings = self.get_settings()
    settings.add_pattern('arch', settings.out_archivefilepatterns)
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(!searcher.is_archive_search_file(f))
  end

  def test_is_archive_search_file_no_match_out_pattern
    settings = self.get_settings()
    settings.add_pattern('archives', settings.out_archivefilepatterns)
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(searcher.is_archive_search_file(f))
  end

################################################################################
# filter_file tests
################################################################################
  def test_filter_file_matches_by_default
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(searcher.filter_file(f))
  end

  def test_filter_file_is_search_file
    settings = self.get_settings()
    settings.add_exts('rb', settings.in_extensions)
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(searcher.filter_file(f))
  end

  def test_filter_file_not_is_search_file
    settings = self.get_settings()
    settings.add_exts('pl', settings.in_extensions)
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(!searcher.filter_file(f))
  end

  def test_filter_file_is_hidden_file
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    f = '.gitignore'
    assert(!searcher.filter_file(f))
  end

  def test_filter_file_hidden_includehidden
    settings = self.get_settings()
    settings.excludehidden = false
    searcher = Searcher.new(settings)
    f = '.gitignore'
    assert(searcher.filter_file(f))
  end

  def test_filter_file_archive_no_searcharchives
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(!searcher.filter_file(f))
  end

  def test_filter_file_archive_searcharchives
    settings = self.get_settings()
    settings.searcharchives = 1
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(searcher.filter_file(f))
  end

  def test_filter_file_archive_archivesonly
    settings = self.get_settings()
    settings.archivesonly = true
    settings.searcharchives = true
    searcher = Searcher.new(settings)
    f = 'archive.zip'
    assert(searcher.filter_file(f))
  end

  def test_filter_file_nonarchive_archivesonly
    settings = self.get_settings()
    settings.archivesonly = true
    settings.searcharchives = true
    searcher = Searcher.new(settings)
    f = 'fileutil.rb'
    assert(!searcher.filter_file(f))
  end

################################################################################
# search_lines tests
################################################################################
  def test_search_lines
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    testfile = self.get_test_file()
    contents = File.open(testfile, mode: 'r:ISO8859-1').read
    results = searcher.search_multiline_string(contents)
    assert_equal(results.count, 2)

    firstResult = results[0]
    assert_equal(firstResult.linenum, 23)
    assert_equal(firstResult.match_start_index, 3)
    assert_equal(firstResult.match_end_index, 11)

    secondResult = results[1]
    assert_equal(secondResult.linenum, 29)
    assert_equal(secondResult.match_start_index, 24)
    assert_equal(secondResult.match_end_index, 32)
  end

################################################################################
# search_multiline_string tests
################################################################################
  def test_search_multiline_string
    settings = self.get_settings()
    searcher = Searcher.new(settings)
    testfile = self.get_test_file()
    fo = File.open(testfile, mode: 'r:ISO8859-1')
    line_iterator = fo.each_line
    results = searcher.search_line_iterator(line_iterator)
    assert_equal(results.count, 2)

    firstResult = results[0]
    assert_equal(firstResult.linenum, 23)
    assert_equal(firstResult.match_start_index, 3)
    assert_equal(firstResult.match_end_index, 11)

    secondResult = results[1]
    assert_equal(secondResult.linenum, 29)
    assert_equal(secondResult.match_start_index, 24)
    assert_equal(secondResult.match_end_index, 32)
  end
end
