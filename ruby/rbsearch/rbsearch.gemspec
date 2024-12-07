# frozen_string_literal: true

require_relative 'lib/rbsearch/version'

Gem::Specification.new do |spec|
  spec.name        = 'rbsearch'
  spec.version     = '0.1.0'
  spec.authors     = ['Cary Clark']
  spec.email       = ['clarkcb@gmail.com']

  spec.summary     = 'ruby version of xsearch'
  spec.homepage    = 'http://github.com/clarkcb/xsearch'
  spec.license     = 'MIT'
  spec.required_ruby_version = ">= 3.3.0"

  spec.metadata['allowed_push_host'] = "TODO: Set to your gem server 'https://example.com'"

  spec.metadata["homepage_uri"]    = spec.homepage
  spec.metadata["source_code_uri"] = spec.homepage
  # spec.metadata["changelog_uri"]   = "TODO: Put your gem's CHANGELOG.md URL here."

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files = Dir.chdir(__dir__) do
    gitfiles = `git ls-files -z`.split("\x0").reject do |f|
      (File.expand_path(f) == __FILE__) ||
        f.start_with?(*%w[test/ spec/ features/ .git .github appveyor Gemfile])
    end
    datafiles = `find ./data -type f -name "*.json" -print0`.split("\x0")
    gitfiles.concat(datafiles)
  end
  spec.test_files = Dir.chdir(__dir__) do
    `git ls-files -z`.split("\x0").select { |f| f.start_with?('test/') } 
  end
  spec.bindir      = 'bin'
  # spec.executables = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  # spec.executables << 'rbsearch'
  spec.executables = %w[rbsearch.sh rbsearch.ps1]
  spec.require_paths = ['lib']

  # Uncomment to register a new dependency of your gem
  # spec.add_dependency "example-gem", "~> 1.0"

  # https://stackoverflow.com/a/19363523/1002072
  spec.add_runtime_dependency 'rbfind'

  # For more information and examples about making a new gem, check out our
  # guide at: https://bundler.io/guides/creating_gem.html
end
