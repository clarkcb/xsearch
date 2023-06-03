# frozen_string_literal: true

module RbSearch

  # FileUtil - file utility functions
  module FileUtil
    module_function

    def dot_dirs
      %w[. .. ./ ../]
    end

    def dot_dir?(file_name)
      f = File.basename(file_name)
      dot_dirs.include?(f)
    end

    def get_extension(file_name)
      ext = ''
      f = File.basename(file_name)
      index = f.rindex('.')
      if index&.positive? && index < (f.length - 1)
        ext = f[index + 1..f.length].downcase
      end
      ext
    end

    def hidden?(file_name)
      f = File.basename(file_name)
      if f.length > 1 && f[0] == '.' && !dot_dirs.include?(f)
        true
      else
        false
      end
    end
  end
end
