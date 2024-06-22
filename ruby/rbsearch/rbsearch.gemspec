# frozen_string_literal: true

require_relative 'lib/rbsearch/version'

Gem::Specification.new do |spec|
  spec.name        = 'rbsearch'
  spec.version     = '0.1.0'
  spec.authors     = ['Cary Clark']
  spec.email       = 'clarkcb@gmail.com'

  spec.summary     = 'ruby version of xsearch'
  spec.homepage    = 'http://github.com/clarkcb/xsearch'
  spec.license     = 'MIT'
  spec.required_ruby_version = ">= 3.3.0"

  spec.metadata["homepage_uri"]    = spec.homepage
  spec.metadata["source_code_uri"] = spec.homepage

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files = Dir.chdir(__dir__) do
    `git ls-files -z`.split("\x0").reject do |f|
      (File.expand_path(f) == __FILE__) ||
        f.start_with?(*%w[bin/ test/ spec/ features/ .git .github appveyor Gemfile])
    end
  end
  # spec.files = Dir.chdir(File.expand_path('..', __FILE__)) do
  #   `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }
  # end
  spec.test_files = Dir.chdir(__dir__) do
    `git ls-files -z`.split("\x0").select { |f| f.start_with?('test/') } 
  end
  # spec.test_files  = spec.files.grep(%r{^(test|spec|features)/})
  spec.bindir      = 'bin'
  spec.executables << 'rbsearch'
  spec.require_paths = ['lib']

  # https://stackoverflow.com/a/19363523/1002072
  spec.add_runtime_dependency 'rbfind'
end
