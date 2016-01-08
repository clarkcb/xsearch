################################################################################
#
# config.rb
#
# Configuration values
#
################################################################################

require 'json'
require 'pathname'
require 'rbconfig'

currdir = File.absolute_path(File.dirname(__FILE__))
config_rel_path = '../../shared/config.json'
config_json_path = Pathname.new(currdir).join(config_rel_path).to_s
file = File.read(config_json_path)
config = JSON.parse(file)

XSEARCHPATH = config['xsearchpath']
SHAREDPATH = "#{XSEARCHPATH}/shared"
FILETYPESPATH = "#{SHAREDPATH}/filetypes.xml"
SEARCHOPTIONSPATH = "#{SHAREDPATH}/searchoptions.xml"
