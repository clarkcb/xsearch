################################################################################
#
# filetypes_test.rb
#
# Test the FileTypes class
#
################################################################################

require_relative "../rbsearch/filetypes.rb"
require "test/unit"
 
class FileTypesTest < Test::Unit::TestCase
	def setup
		@filetypes = FileTypes.new
	end

  def test_get_filetype_archive_file
    filename = 'archive.zip'
    assert_equal(@filetypes.get_filetype(filename), FileType::Archive)
  end

  def test_get_filetype_binary_file
    filename = 'binary.exe'
    assert_equal(@filetypes.get_filetype(filename), FileType::Binary)
  end

  def test_get_filetype_text_file
    filename = 'text.txt'
    assert_equal(@filetypes.get_filetype(filename), FileType::Text)
  end

  def test_get_filetype_unknown_file
    filename = 'unknown.xyz'
    assert_equal(@filetypes.get_filetype(filename), FileType::Unknown)
  end
end
