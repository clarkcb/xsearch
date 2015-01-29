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
    f = File.basename(filename)
    index = f.rindex('.')
    if index and index > 0 and index < (f.length - 1)
      ext = f[index+1..f.length].downcase
    end
    ext
  end

  def self.is_hidden?(filename)
    f = File.basename(filename)
    if f.length > 1 and f[0] == '.' and ! ['.', '..'].include?(f)
      true
    else
      false
    end
  end
end
