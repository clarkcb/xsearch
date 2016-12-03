###############################################################################
#
# FileType.pm
#
# Enum of file types
#
###############################################################################

package plsearch::FileType;

use strict;
use warnings;

use constant {
    UNKNOWN => 'Unknown',
    ARCHIVE => 'Archive',
    BINARY  => 'Binary',
    TEXT    => 'Text',
};

1;

__END__
