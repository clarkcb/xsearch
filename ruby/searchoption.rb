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
  attr_accessor :desc
  attr_accessor :func

  def initialize(shortarg, longarg, desc, func)
    @shortarg = shortarg
    @longarg = longarg
    @desc = desc
    @func = func
  end

  def sortarg
    if not @shortarg.nil? and not @shortarg.empty?
        @shortarg.downcase
    else
      @longarg.downcase
    end
  end

end
