# frozen_string_literal: true

require 'json'
require_relative 'config.rb'
require_relative 'fileutil.rb'

module FileType
  UNKNOWN = 0
  ARCHIVE = 1
  BINARY  = 2
  CODE    = 3
  TEXT    = 4
  XML     = 5
end

# FileTypes - provides basic file type information
class FileTypes
  FILE_TYPE_NAMES = %w[UNKNOWN ARCHIVE BINARY CODE TEXT XML].freeze

  def initialize
    set_filetype_map_from_json
  end

  def self.from_name(name)
    idx = FILE_TYPE_NAMES.index(name.upcase)
    idx.nil? ? 0 : idx
  end

  def self.to_name(filetype)
    filetype < FILE_TYPE_NAMES.size ? FILE_TYPE_NAMES[filetype] : 0
  end

  def set_filetype_map_from_json
    @file_type_map = {}
    f = File.open(File.expand_path(FILETYPESJSONPATH), mode: 'r')
    json = f.read
    json_hash = JSON.parse(json)
    json_hash['filetypes'].each do |ft|
      typename = ft['type']
      exts = ft['extensions'].to_set
      @file_type_map[typename] = exts
    end
    @file_type_map['text'] = @file_type_map['text'] + @file_type_map['code'] +
                             @file_type_map['xml']
    @file_type_map['searchable'] = @file_type_map['text'] +
                                   @file_type_map['archive'] +
                                   @file_type_map['binary']
  rescue StandardError => e
    raise SearchError, "#{e} (file: #{SEARCHOPTIONSJSONPATH})"
  ensure
    f&.close
  end

  def get_filetype(filename)
    if code_file?(filename)
      FileType::CODE
    elsif xml_file?(filename)
      FileType::XML
    elsif text_file?(filename)
      FileType::TEXT
    elsif binary_file?(filename)
      FileType::BINARY
    elsif archive_file?(filename)
      FileType::ARCHIVE
    else
      FileType::UNKNOWN
    end
  end

  def archive_file?(filename)
    @file_type_map['archive'].include?(FileUtil.get_extension(filename))
  end

  def binary_file?(filename)
    @file_type_map['binary'].include?(FileUtil.get_extension(filename))
  end

  def code_file?(filename)
    @file_type_map['code'].include?(FileUtil.get_extension(filename))
  end

  def searchable_file?(filename)
    @file_type_map['searchable'].include?(FileUtil.get_extension(filename))
  end

  def text_file?(filename)
    @file_type_map['text'].include?(FileUtil.get_extension(filename))
  end

  def xml_file?(filename)
    @file_type_map['xml'].include?(FileUtil.get_extension(filename))
  end
end
