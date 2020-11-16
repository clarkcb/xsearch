require_relative 'lib/rbsearch/version'

Gem::Specification.new do |s|
  s.name        = 'rbsearch'
  s.version     = '0.1.0'
  s.authors     = ['Cary Clark']
  s.email       = 'clarkcb@gmail.com'

  s.summary     = 'ruby version of xsearch'
  s.homepage    = 'http://github.com/clarkcb/xsearch'
  s.license     = 'MIT'
  s.required_ruby_version = Gem::Requirement.new('>= 2.3.0')

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  s.files       = Dir.chdir(File.expand_path('..', __FILE__)) do
    `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }
  end
  s.bindir      = 'bin'
  s.executables << 'rbsearch'
  s.require_paths = ['lib']
end
