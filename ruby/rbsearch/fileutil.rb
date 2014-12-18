################################################################################
#
# fileutil.rb
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
################################################################################
require 'rexml/document'
include REXML

class FileUtil
  def initialize
    # TODO: move to config
    @file_types_path = '/Users/cary/src/git/xsearch/shared/filetypes.xml'
    set_file_type_map
  end

  def set_file_type_map
    @file_type_map = {}
    doc = Document.new(File.new(@file_types_path))
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

  def get_extension(filename)
    ext = ''
    index = filename.rindex('.')
    if index and index < (filename.length - 1)
      ext = filename[index+1..filename.length].downcase
    end
    ext
  end

  def is_archive_file(f)
    @file_type_map['archive'].include?(get_extension(f))
  end

  def is_binary_file(f)
    @file_type_map['binary'].include?(get_extension(f))
  end

  def is_searchable_file(f)
    @file_type_map['searchable'].include?(get_extension(f))
  end

  def is_text_file(f)
    @file_type_map['text'].include?(get_extension(f))
  end

end
