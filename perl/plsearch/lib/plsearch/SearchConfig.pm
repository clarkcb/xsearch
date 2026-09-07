###############################################################################
#
# SearchConfig.pm
#
# Configuration values
#
###############################################################################

package plsearch::SearchConfig;

use lib $ENV{'XFIND_PATH'} . '/perl/plfind/lib';

# use parent 'Exporter';

use Path::Class;

use plfind::FileUtil;
use plfind::FindConfig;

our @ISA = 'plfind::FindConfig';

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = $class->SUPER::new();

    my $xsearch_config_dir;
    if (defined $ENV{XSEARCH_CONFIG_DIR}) {
        $xsearch_config_dir = dir($ENV{'XSEARCH_CONFIG_DIR'})
    } else {
        $xsearch_config_dir = dir($ENV{'HOME'}, '.config', 'xsearch');
    }

    my $xsearch_path;
    if (defined $ENV{XSEARCH_PATH}) {
        $xsearch_path = dir($ENV{'XSEARCH_PATH'})
    } else {
        $xsearch_path = dir($ENV{'HOME'}, 'src', 'xsearch');
    }
    my $shared_path = $xsearch_path->subdir('shared');
    my $search_options_path = $shared_path->file('searchoptions.json');
    my $default_search_settings_path = file($xsearch_config_dir, 'settings.json');

    $self->{xsearch_path} = $xsearch_path;
    $self->{search_options_path} = $search_options_path;
    $self->{default_search_settings_path} = $default_search_settings_path;

    bless $self, $class;
    return $self;
}

1;

__END__
