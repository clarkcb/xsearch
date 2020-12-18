# frozen_string_literal: true

module RbSearch

  # SearchError - custom exception class
  class SearchError < StandardError
    def initialize(msg = 'Searcherror occurred')
      super(msg)
    end
  end
end
