################################################################################
#
# fileutil.rb
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
################################################################################

class FileUtil
  def self.get_extension(filename)
    ext = ''
    index = filename.rindex('.')
    if index and index > 0 and index < (filename.length - 1)
      ext = filename[index+1..filename.length].downcase
    end
    ext
  end
end
