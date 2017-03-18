################################################################################
#
# filetypes.rb
#
# Provides information on file type (archive, binary, text, unknown)
#
################################################################################
require 'rexml/document'
require_relative 'config.rb'
require_relative 'fileutil.rb'
include REXML

module FileType
  Unknown = 0
  Archive = 1
  Binary  = 2
  Code    = 3
  Text    = 4
  Xml     = 5
end

class FileTypes
  def initialize
    set_file_type_map
  end

  def self.from_name(name)
    uname = name.upcase()
    filetype = FileType::Unknown
    if uname == "TEXT"
      filetype = FileType::Text
    elsif uname == "BINARY"
      filetype = FileType::Binary
    elsif uname == "ARCHIVE"
      filetype = FileType::Archive
    elsif uname == "CODE"
      filetype = FileType::Code
    elsif uname == "XML"
      filetype = FileType::Xml
    end
    filetype
  end

  def set_file_type_map
    @file_type_map = {}
    doc = Document.new(File.new(File.expand_path(FILETYPESPATH)))
    doc.elements.each('filetypes/filetype') { |filetype|
      name = filetype.attributes['name']
      filetype.elements.each('extensions') { |extensions|
        exts = extensions.text.split(' ')
        @file_type_map[name] = exts.to_set
      }
    }
    @file_type_map['text'] = @file_type_map['text'] + @file_type_map['code'] +
      @file_type_map['xml']
    @file_type_map['searchable'] = @file_type_map['text'] +
      @file_type_map['archive'] + @file_type_map['binary']
  end

  def get_filetype(filename)
    if is_text_file(filename)
      FileType::Text
    elsif is_binary_file(filename)
      FileType::Binary
    elsif is_archive_file(filename)
      FileType::Archive
    else
      FileType::Unknown
    end
  end

  def is_archive_file(f)
    @file_type_map['archive'].include?(FileUtil::get_extension(f))
  end

  def is_binary_file(f)
    @file_type_map['binary'].include?(FileUtil::get_extension(f))
  end

  def is_code_file(f)
    @file_type_map['code'].include?(FileUtil::get_extension(f))
  end

  def is_searchable_file(f)
    @file_type_map['searchable'].include?(FileUtil::get_extension(f))
  end

  def is_text_file(f)
    @file_type_map['text'].include?(FileUtil::get_extension(f))
  end

  def is_xml_file(f)
    @file_type_map['xml'].include?(FileUtil::get_extension(f))
  end
end
