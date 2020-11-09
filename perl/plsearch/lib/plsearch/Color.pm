###############################################################################
#
# Color.pm
#
# Enum of (console) color values
#
###############################################################################

package plsearch::Color;

use strict;
use warnings;

use constant {
    GREY   => "\033[30m",
    RED    => "\033[31m",
    GREEN  => "\033[32m",
    YELLOW => "\033[33m",
    BLUE   => "\033[34m",
    RESET  => "\033[0m",
};

1;

__END__
