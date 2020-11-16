# frozen_string_literal: true

# SearchFile - encapsulates a file to be searched
class SearchFile

  attr_accessor :containers
  attr_reader :path
  attr_reader :filename
  attr_reader :filetype

  CONTAINER_SEPARATOR = '!'

  def initialize(path, filename, filetype)
    @containers = []
    @path = path
    @filename = filename
    @filetype = filetype # FileType
  end

  def relativepath
    return '.' + File::SEPARATOR + @filename if @path == '.' || @path == './'

    Pathname.new(@path).join(@filename).to_s
  end

  def to_s
    s = ''
    unless @containers.empty?
      s += @containers.join(CONTAINER_SEPARATOR) + CONTAINER_SEPARATOR
    end
    s + relativepath
  end

end
