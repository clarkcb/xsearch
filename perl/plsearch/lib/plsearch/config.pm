###############################################################################
#
# config.pm
#
# Configuration values
#
###############################################################################

package plsearch::config;

use strict;
use warnings;

use parent 'Exporter';

use Cwd qw(abs_path);
use File::Basename;
use File::Spec;
use JSON::PP qw(decode_json);

use plsearch::FileUtil;

my $absdir = dirname(abs_path(__FILE__));
my $share_path = File::Spec->join($absdir, '../../share');
my $config_json_path = $share_path . '/config.json';
my $config = decode_json plsearch::FileUtil::get_file_contents($config_json_path);

our $XSEARCHPATH = $config->{xsearchpath};
our $SHAREDPATH = "$XSEARCHPATH/shared";
our $FILETYPESPATH = "$share_path/filetypes.json";
our $SEARCHOPTIONSPATH = "$share_path/searchoptions.json";

our @EXPORT = qw($XSEARCHPATH $SHAREDPATH $FILETYPESPATH $SEARCHOPTIONSPATH);

1;

__END__
