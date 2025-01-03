###############################################################################
#
# SearchResult.pm
#
# Encapsulates a search result
#
###############################################################################

package plsearch::SearchResult;

use strict;
use warnings;

use plsearch::Color;

sub new {
    # file is a plfind::FileResult instance
    my $class = shift;
    my $self = {
        pattern => shift,
        file => shift,
        line_num => shift,
        match_start_index => shift,
        match_end_index => shift,
        line => shift,
        lines_before => shift || [],
        lines_after => shift || [],
    };
    bless $self, $class;
    return $self;
}

1;

__END__
