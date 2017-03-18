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
    CODE    => 'Code',
    TEXT    => 'Text',
    XML     => 'Xml',
};

1;

__END__
