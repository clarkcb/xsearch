# frozen_string_literal: true

module RbSearch

  # SearchOption - encapsulates a CLI search option
  class SearchOption
    attr_reader :short_arg
    attr_reader :long_arg
    attr_reader :desc
    attr_reader :func

    def initialize(short_arg, long_arg, desc, func)
      @short_arg = short_arg
      @long_arg = long_arg
      @desc = desc
      @func = func
    end

    def sort_arg
      if !@short_arg.nil? && !@short_arg.empty?
        @short_arg.downcase + 'a' + @long_arg.downcase
      else
        @long_arg.downcase
      end
    end
  end
end
