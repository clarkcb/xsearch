# frozen_string_literal: true

module RbSearch
  # SearchFile - encapsulates a file to be searched
  class SearchFile

    attr_accessor :containers
    attr_reader :path
    attr_reader :file_name
    attr_reader :file_type

    CONTAINER_SEPARATOR = '!'

    def initialize(path, file_name, file_type)
      @containers = []
      @path = path
      @file_name = file_name
      @file_type = file_type # FileType
    end

    def relative_path
      return '.' + File::SEPARATOR + @file_name if @path == '.' || @path == './'

      Pathname.new(@path).join(@file_name).to_s
    end

    def to_s
      s = ''
      unless @containers.empty?
        s += @containers.join(CONTAINER_SEPARATOR) + CONTAINER_SEPARATOR
      end
      s + relative_path
    end
  end
end
