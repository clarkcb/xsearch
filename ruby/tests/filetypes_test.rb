################################################################################
#
# filetypes_test.rb
#
# Test the FileTypes class
#
################################################################################

require_relative '../rbsearch/filetypes.rb'
require 'test/unit'

class FileTypesTest < Test::Unit::TestCase
  def setup
    @filetypes = FileTypes.new
  end

  def test_get_filetype_archive_file
    filename = 'archive.zip'
    assert_equal(@filetypes.get_filetype(filename), FileType::ARCHIVE)
  end

  def test_get_filetype_binary_file
    filename = 'binary.exe'
    assert_equal(@filetypes.get_filetype(filename), FileType::BINARY)
  end

  def test_get_filetype_code_file
    filename = 'code.rb'
    assert_equal(@filetypes.get_filetype(filename), FileType::CODE)
  end

  def test_get_filetype_text_file
    filename = 'text.txt'
    assert_equal(@filetypes.get_filetype(filename), FileType::TEXT)
  end

  def test_get_filetype_xml_file
    filename = 'markup.xml'
    assert_equal(@filetypes.get_filetype(filename), FileType::XML)
  end

  def test_get_filetype_searchable_file
    filename = 'config.ini'
    assert_true(@filetypes.searchable_file?(filename))
  end

  def test_get_filetype_unknown_file
    filename = 'unknown.xyz'
    assert_equal(@filetypes.get_filetype(filename), FileType::UNKNOWN)
  end
end
