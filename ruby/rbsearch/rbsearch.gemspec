require_relative 'lib/rbsearch/version'

Gem::Specification.new do |spec|
  spec.name        = 'rbsearch'
  spec.version     = '0.1.0'
  spec.authors     = ['Cary Clark']
  spec.email       = 'clarkcb@gmail.com'

  spec.summary     = 'ruby version of xsearch'
  spec.homepage    = 'http://github.com/clarkcb/xsearch'
  spec.license     = 'MIT'
  spec.required_ruby_version = Gem::Requirement.new('>= 2.7.2')

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files       = Dir.chdir(File.expand_path('..', __FILE__)) do
    `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }
  end
  spec.test_files  = spec.files.grep(%r{^(test|spec|features)/})
  spec.bindir      = 'bin'
  spec.executables << 'rbsearch'
  spec.require_paths = ['lib']

  # https://stackoverflow.com/a/19363523/1002072
  spec.add_runtime_dependency "rbfind"
end
