# frozen_string_literal: true

# FileUtil - file utility functions
module FileUtil
  module_function

  def dot_dirs
    %w[. .. ./ ../]
  end

  def dot_dir?(filename)
    f = File.basename(filename)
    dot_dirs.include?(f)
  end

  def get_extension(filename)
    ext = ''
    f = File.basename(filename)
    index = f.rindex('.')
    if index&.positive? && index < (f.length - 1)
      ext = f[index + 1..f.length].downcase
    end
    ext
  end

  def hidden?(filename)
    f = File.basename(filename)
    if f.length > 1 && f[0] == '.' && !dot_dirs.include?(f)
      true
    else
      false
    end
  end
end
