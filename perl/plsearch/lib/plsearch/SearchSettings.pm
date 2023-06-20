###############################################################################
#
# SearchSettings.pm
#
# Encapsulates the search settings
#
###############################################################################

package plsearch::SearchSettings;

use lib $ENV{'XFIND_PATH'} . '/perl/plfind/lib';

use plfind::FindSettings;
use plfind::common;

our @ISA = 'plfind::FindSettings';

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = $class->SUPER::new();

    # Add the search-specific settings
    $self->{colorize} = 1;
    $self->{first_match} = 0;
    $self->{in_lines_after_patterns} = [];
    $self->{in_lines_before_patterns} = [];
    $self->{lines_after} = 0;
    $self->{lines_after_to_patterns} = [];
    $self->{lines_after_until_patterns} = [];
    $self->{lines_before} = 0;
    $self->{list_lines} = 0;
    $self->{max_line_length} = 150;
    $self->{multi_line_search} = 0;
    $self->{out_lines_after_patterns} = [];
    $self->{out_lines_before_patterns} = [];
    $self->{print_results} = 1;
    $self->{search_archives} = 0;
    $self->{search_patterns} = [];
    $self->{text_file_encoding} = 'UTF-8';
    $self->{unique_lines} = 0;

    bless $self, $class;
    return $self;
}

sub to_string {
    my $self = shift @_;
    my $s = "SearchSettings(" .
        'archives_only=' . $self->bool_to_string($self->{archives_only}) .
        ', colorize=' . $self->bool_to_string($self->{colorize}) .
        ', debug=' . $self->bool_to_string($self->{debug}) .
        ', exclude_hidden=' . $self->bool_to_string($self->{exclude_hidden}) .
        ', first_match=' . $self->bool_to_string($self->{first_match}) .
        ', in_archive_extensions=' . $self->strings_aref_to_string($self->{in_archive_extensions}) .
        ', in_archive_file_patterns=' . $self->strings_aref_to_string($self->{in_archive_file_patterns}) .
        ', in_dir_patterns=' . $self->strings_aref_to_string($self->{in_dir_patterns}) .
        ', in_extensions=' . $self->strings_aref_to_string($self->{in_extensions}) .
        ', in_file_patterns=' . $self->strings_aref_to_string($self->{in_file_patterns}) .
        ', in_file_types=' . $self->file_types_aref_to_string($self->{in_file_types}) .
        ', in_lines_after_patterns=' . $self->strings_aref_to_string($self->{in_lines_after_patterns}) .
        ', in_lines_before_patterns=' . $self->strings_aref_to_string($self->{in_lines_before_patterns}) .
        ', lines_after=' . $self->{lines_after} .
        ', lines_after_to_patterns=' . $self->strings_aref_to_string($self->{lines_after_to_patterns}) .
        ', lines_after_until_patterns=' . $self->strings_aref_to_string($self->{lines_after_until_patterns}) .
        ', lines_before=' . $self->{lines_before} .
        ', list_dirs=' . $self->bool_to_string($self->{list_dirs}) .
        ', list_files=' . $self->bool_to_string($self->{list_files}) .
        ', list_lines=' . $self->bool_to_string($self->{list_lines}) .
        ', max_last_mod=' . $self->datetime_to_string($self->{max_last_mod}) .
        ', max_line_length=' . $self->{max_line_length} .
        ', max_size=' . $self->{max_size} .
        ', min_last_mod=' . $self->datetime_to_string($self->{min_last_mod}) .
        ', min_size=' . $self->{min_size} .
        ', multi_line_search=' . $self->bool_to_string($self->{multi_line_search}) .
        ', out_archive_extensions=' . $self->strings_aref_to_string($self->{out_archive_extensions}) .
        ', out_archive_file_patterns=' . $self->strings_aref_to_string($self->{out_archive_file_patterns}) .
        ', out_dir_patterns=' . $self->strings_aref_to_string($self->{out_dir_patterns}) .
        ', out_extensions=' . $self->strings_aref_to_string($self->{out_extensions}) .
        ', out_file_patterns=' . $self->strings_aref_to_string($self->{out_file_patterns}) .
        ', out_file_types=' . $self->file_types_aref_to_string($self->{out_file_types}) .
        ', out_lines_after_patterns=' . $self->strings_aref_to_string($self->{out_lines_after_patterns}) .
        ', out_lines_before_patterns=' . $self->strings_aref_to_string($self->{out_lines_before_patterns}) .
        ', paths=' . $self->strings_aref_to_string($self->{paths}) .
        ', print_results=' . $self->bool_to_string($self->{print_results}) .
        ', print_usage=' . $self->bool_to_string($self->{print_usage}) .
        ', print_version=' . $self->bool_to_string($self->{print_version}) .
        ', recursive=' . $self->bool_to_string($self->{recursive}) .
        ', search_archives=' . $self->bool_to_string($self->{search_archives}) .
        ', search_patterns=' . $self->strings_aref_to_string($self->{search_patterns}) .
        ', sort_by=' . $self->{sort_by} .
        ', sort_case_insensitive=' . $self->bool_to_string($self->{sort_case_insensitive}) .
        ', sort_descending=' . $self->bool_to_string($self->{sort_descending}) .
        ', text_file_encoding="' . $self->{text_file_encoding} . '"' .
        ', unique_lines=' . $self->bool_to_string($self->{unique_lines}) .
        ', verbose=' . $self->bool_to_string($self->{verbose}) .
        ')';
    return $s;
}

1;

__END__
