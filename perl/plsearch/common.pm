###############################################################################
#
# common.pm
#
# Common functions
#
###############################################################################

package plsearch::common;

use strict;

sub log($) {
    my $msg = shift;
    print $msg . "\n";
}

sub log_msg($) {
    my $msg = shift;
    print $msg . "\n";
}

sub trim($) {
    my $string = shift;
    $string =~ s/^\s+//;
    $string =~ s/\s+$//;
    return $string;
}

1;

__END__
