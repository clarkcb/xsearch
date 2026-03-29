###############################################################################
#
# SearchSettings.pm
#
# Encapsulates the search settings
#
###############################################################################

package plsearch::SearchSettings;

use lib $ENV{'XFIND_PATH'} . '/perl/plfind/lib';

use plfind::common;
use plfind::Color;
use plfind::FindSettings;

our @ISA = 'plfind::FindSettings';

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = $class->SUPER::new();

    # Add the search-specific settings
    $self->{first_match} = 0;
    $self->{in_lines_after_patterns} = [];
    $self->{in_lines_before_patterns} = [];
    $self->{line_color} = plfind::Color->GREEN;
    $self->{lines_after} = 0;
    $self->{lines_after_to_patterns} = [];
    $self->{lines_after_until_patterns} = [];
    $self->{lines_before} = 0;
    $self->{max_line_length} = 150;
    $self->{multi_line_search} = 0;
    $self->{out_lines_after_patterns} = [];
    $self->{out_lines_before_patterns} = [];
    $self->{print_lines} = 0;
    $self->{print_matches} = 0;
    $self->{print_results} = 1;
    $self->{search_archives} = 0;
    $self->{search_patterns} = [];
    $self->{text_file_encoding} = 'UTF-8';
    $self->{unique_lines} = 0;

    bless $self, $class;
    return $self;
}

1;

__END__
