################################################################################
#
# config.rb
#
# Configuration values
#
################################################################################

require 'rbconfig'

HOME_NAME = 'HOME'
if RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/
  HOME_NAME = 'USERPROFILE'
end

HOME = ENV[HOME_NAME]
XSEARCHPATH = "#{HOME}/src/git/xsearch"
SHAREDPATH = "#{XSEARCHPATH}/shared"
FILETYPESPATH = "#{SHAREDPATH}/filetypes.xml"
SEARCHOPTIONSPATH = "#{SHAREDPATH}/searchoptions.xml"
