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

# use Cwd qw(abs_path);
# use File::Basename;
# use File::Spec;
# use JSON::PP qw(decode_json);
use Path::Class;

our $XSEARCH_PATH = $ENV{'XSEARCH_PATH'};
if (defined $ENV{XSEARCH_PATH}) {
    $XSEARCH_PATH = dir($ENV{'XSEARCH_PATH'})
} else {
    $XSEARCH_PATH = dir($ENV{'HOME'}, 'src', 'xsearch');
}
our $SHARED_PATH = $XSEARCH_PATH->subdir('shared');
our $SEARCH_OPTIONS_PATH = $SHARED_PATH->file('searchoptions.json');

our @EXPORT = qw($XSEARCH_PATH $SHARED_PATH $SEARCH_OPTIONS_PATH);

1;

__END__
