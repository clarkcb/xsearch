# frozen_string_literal: true

# SearchError - custom exception class
class SearchError < StandardError
  def initialize(msg = 'Searcherror occurred')
    super(msg)
  end
end
