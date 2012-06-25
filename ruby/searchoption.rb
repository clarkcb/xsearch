################################################################################
#
# searchoption.rb
#
# class SearchOption: encapsulates a (command-line) search option
#
################################################################################

class SearchOption

  attr_accessor :shortarg
  attr_accessor :longarg
  attr_accessor :func
  attr_accessor :desc

  def initialize(shortarg, longarg, func, desc)
    @shortarg = shortarg
    @longarg = longarg
    @func = func
    @desc = desc
  end

  def sortarg
    if not @shortarg.nil? and not @shortarg.empty?
        @shortarg.downcase
    else
      @longarg.downcase
    end
  end

end
