# frozen_string_literal: true

################################################################################
#
# config.rb
#
# Configuration values
#
################################################################################

require 'json'
require 'pathname'

currdir = File.absolute_path(File.dirname(__FILE__))
config_rel_path = '../../shared/config.json'
config_json_path = Pathname.new(currdir).join(config_rel_path).to_s
file = File.read(config_json_path)
config = JSON.parse(file)

XSEARCHPATH = config['xsearchpath']
SHAREDPATH = "#{XSEARCHPATH}/shared".freeze
FILETYPESPATH = "#{SHAREDPATH}/filetypes.xml".freeze
FILETYPESJSONPATH = "#{SHAREDPATH}/filetypes.json".freeze
SEARCHOPTIONSPATH = "#{SHAREDPATH}/searchoptions.xml".freeze
SEARCHOPTIONSJSONPATH = "#{SHAREDPATH}/searchoptions.json".freeze
