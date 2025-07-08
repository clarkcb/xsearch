###############################################################################
#
# FileResultSorter.pm
#
# Sorts file results
#
###############################################################################

package plsearch::SearchResultSorter;

use strict;
use warnings;

use plfind::FileResultSorter;
use plfind::SortBy;


sub new {
    my $class = shift;
    my $settings = shift;
    my $self = {
        settings => $settings,
        file_result_sorter => plfind::FileResultSorter->new($settings),
    };
    bless $self, $class;
    return $self;
}

sub cmp_by_search_fields {
    my ($sr1, $sr2) = @_;
    if ($sr1->{line_num} == $sr2->{line_num}) {
        if ($sr1->{match_start_index} == $sr2->{match_start_index}) {
            return $sr1->{match_end_index} <=> $sr2->{match_end_index};
        }
        return $sr1->{match_start_index} <=> $sr2->{match_start_index};
    }
    return $sr1->{line_num} <=> $sr2->{line_num};
}

sub cmp_file_results_by_path {
    my ($self, $sr1, $sr2) = @_;
    my $file_cmp = $self->{file_result_sorter}->cmp_file_results_by_path($sr1->{file}, $sr2->{file});
    if ($file_cmp == 0) {
        return cmp_by_search_fields($sr1, $sr2);
    }
    return $file_cmp;
}

sub cmp_file_results_by_file_name {
    my ($self, $sr1, $sr2) = @_;
    my $file_cmp = $self->{file_result_sorter}->cmp_file_results_by_file_name($sr1->{file}, $sr2->{file});
    if ($file_cmp == 0) {
        return cmp_by_search_fields($sr1, $sr2);
    }
    return $file_cmp;
}

sub cmp_file_results_by_file_size {
    my ($self, $sr1, $sr2) = @_;
    my $file_cmp = $self->{file_result_sorter}->cmp_file_results_by_file_size($sr1->{file}, $sr2->{file});
    if ($file_cmp == 0) {
        return cmp_by_search_fields($sr1, $sr2);
    }
    return $file_cmp;
}

sub cmp_file_results_by_file_type {
    my ($self, $sr1, $sr2) = @_;
    my $file_cmp = $self->{file_result_sorter}->cmp_file_results_by_file_type($sr1->{file}, $sr2->{file});
    if ($file_cmp == 0) {
        return cmp_by_search_fields($sr1, $sr2);
    }
    return $file_cmp;
}

sub cmp_file_results_by_last_mod {
    my ($self, $sr1, $sr2) = @_;
    my $file_cmp = $self->{file_result_sorter}->cmp_file_results_by_last_mod($sr1->{file}, $sr2->{file});
    if ($file_cmp == 0) {
        return cmp_by_search_fields($sr1, $sr2);
    }
    return $file_cmp;
}

sub sort {
    my ($self, $search_results) = @_;
    my @sorted;
    if ($self->{settings}->{sort_descending}) {
        if ($self->{settings}->{sort_by} eq plfind::SortBy->FILENAME) {
            @sorted = sort {$self->cmp_file_results_by_file_name($b, $a)} @$search_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILESIZE) {
            @sorted = sort {$self->cmp_file_results_by_file_size($b, $a)} @$search_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILETYPE) {
            @sorted = sort {$self->cmp_file_results_by_file_type($b, $a)} @$search_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->LASTMOD) {
            @sorted = sort {$self->cmp_file_results_by_last_mod($b, $a)} @$search_results;
        } else {
            @sorted = sort {$self->cmp_file_results_by_path($b, $a)} @$search_results;
        }
    } else {
        if ($self->{settings}->{sort_by} eq plfind::SortBy->FILENAME) {
            @sorted = sort {$self->cmp_file_results_by_file_name($a, $b)} @$search_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILESIZE) {
            @sorted = sort {$self->cmp_file_results_by_file_size($a, $b)} @$search_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->FILETYPE) {
            @sorted = sort {$self->cmp_file_results_by_file_type($a, $b)} @$search_results;
        } elsif ($self->{settings}->{sort_by} eq plfind::SortBy->LASTMOD) {
            @sorted = sort {$self->cmp_file_results_by_last_mod($a, $b)} @$search_results;
        } else {
            @sorted = sort {$self->cmp_file_results_by_path($a, $b)} @$search_results;
        }
    }
    return \@sorted;
}

1;

__END__
