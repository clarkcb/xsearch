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

my $absdir = dirname(abs_path(__FILE__));
my $share_path = File::Spec->join($absdir, '../../share');

our $XSEARCHPATH = $ENV{'XSEARCH_PATH'};
our $SHAREDPATH = "$XSEARCHPATH/shared";
our $FILETYPESPATH = "$share_path/filetypes.json";
our $SEARCHOPTIONSPATH = "$share_path/searchoptions.json";

our @EXPORT = qw($XSEARCHPATH $SHAREDPATH $FILETYPESPATH $SEARCHOPTIONSPATH);

1;

__END__
