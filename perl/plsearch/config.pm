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

our $HOME = $ENV{HOME};
if ($^O eq 'MSWin32') {
	$HOME = $ENV{USERPROFILE};
}

my $XSEARCHPATH = "$HOME/src/git/xsearch";
my $SHAREDPATH = "$XSEARCHPATH/shared";
our $FILETYPESPATH = "$SHAREDPATH/filetypes.xml";
our $SEARCHOPTIONSPATH = "$SHAREDPATH/searchoptions.xml";

our @EXPORT = qw($HOME $FILETYPESPATH $SEARCHOPTIONSPATH);

1;

__END__
