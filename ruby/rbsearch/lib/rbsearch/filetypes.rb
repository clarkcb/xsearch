# frozen_string_literal: true

require 'json'
require_relative 'fileutil'
require_relative 'searcherror'

module RbSearch

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
      set_file_type_map_from_json
    end

    def self.from_name(name)
      idx = FILE_TYPE_NAMES.index(name.upcase)
      idx.nil? ? 0 : idx
    end

    def self.to_name(file_type)
      file_type < FILE_TYPE_NAMES.size ? FILE_TYPE_NAMES[file_type] : 0
    end

    def set_file_type_map_from_json
      @file_type_map = {}
      file_types_json_path = File.join(File.dirname(__FILE__), "../../data/filetypes.json")
      f = File.open(file_types_json_path, mode: 'r')
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

    def get_file_type(file_name)
      if code_file?(file_name)
        FileType::CODE
      elsif xml_file?(file_name)
        FileType::XML
      elsif text_file?(file_name)
        FileType::TEXT
      elsif binary_file?(file_name)
        FileType::BINARY
      elsif archive_file?(file_name)
        FileType::ARCHIVE
      else
        FileType::UNKNOWN
      end
    end

    def archive_file?(file_name)
      @file_type_map['archive'].include?(FileUtil.get_extension(file_name))
    end

    def binary_file?(file_name)
      @file_type_map['binary'].include?(FileUtil.get_extension(file_name))
    end

    def code_file?(file_name)
      @file_type_map['code'].include?(FileUtil.get_extension(file_name))
    end

    def searchable_file?(file_name)
      @file_type_map['searchable'].include?(FileUtil.get_extension(file_name))
    end

    def text_file?(file_name)
      @file_type_map['text'].include?(FileUtil.get_extension(file_name))
    end

    def xml_file?(file_name)
      @file_type_map['xml'].include?(FileUtil.get_extension(file_name))
    end
  end
end
