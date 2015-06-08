################################################################################
#
# searchfile.rb
#
# class SearchFile: encapsulates a file to be searched
#
################################################################################

class SearchFile

  attr_accessor :containers
  attr_accessor :path
  attr_accessor :filename
  attr_accessor :filetype

  @CONTAINER_SEPARATOR = '!'

  def initialize(path, filename, filetype)
    @containers = []
    @path = path
    @filename = filename
    @filetype = filetype
  end

  def to_s
    s = ""
    if @containers.length > 0
      @containers.join(@CONTAINER_SEPARATOR) + @CONTAINER_SEPARATOR
    end
    s += Pathname.new(@path).join(@filename).to_s
    s
  end

end
