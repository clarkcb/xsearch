###############################################################################
#
# SearchOption.pm
#
# Encapsulates a search option
#
###############################################################################

package plsearch::SearchOption;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        short_arg => shift,
        long_arg => shift,
        desc => shift,
    };
    bless $self, $class;
    return $self;
}

1;

__END__
