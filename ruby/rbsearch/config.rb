################################################################################
#
# config.rb
#
# Configuration values
#
################################################################################

require 'rbconfig'

def windows?
  RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/
end

HOME_NAME = windows? ? 'USERPROFILE' : 'HOME'
HOME = ENV[HOME_NAME]
XSEARCHPATH = "#{HOME}/src/xsearch"
SHAREDPATH = "#{XSEARCHPATH}/shared"
FILETYPESPATH = "#{SHAREDPATH}/filetypes.xml"
SEARCHOPTIONSPATH = "#{SHAREDPATH}/searchoptions.xml"
