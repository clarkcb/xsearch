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

our $FILETYPESPATH = "$HOME/src/git/xsearch/shared/filetypes.xml";
our $SEARCHOPTIONSPATH = "$HOME/src/git/xsearch/shared/searchoptions.xml";

our @EXPORT = qw($HOME $FILETYPESPATH $SEARCHOPTIONSPATH);

1;

__END__
