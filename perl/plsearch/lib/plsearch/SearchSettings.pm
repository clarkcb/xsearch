###############################################################################
#
# SearchSettings.pm
#
# Encapsulates the search settings
#
###############################################################################

package plsearch::SearchSettings;

use plsearch::FileTypes;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        archives_only => 0,
        colorize => 1,
        debug => 0,
        exclude_hidden => 1,
        first_match => 0,
        in_archive_extensions => [],
        in_archive_file_patterns => [],
        in_dir_patterns => [],
        in_extensions => [],
        in_file_patterns => [],
        in_file_types => [],
        in_lines_after_patterns => [],
        in_lines_before_patterns => [],
        lines_after => 0,
        lines_after_to_patterns => [],
        lines_after_until_patterns => [],
        lines_before => 0,
        list_dirs => 0,
        list_files => 0,
        list_lines => 0,
        max_line_length => 150,
        multi_line_search => 0,
        out_archive_extensions => [],
        out_archive_file_patterns => [],
        out_dir_patterns => [],
        out_extensions => [],
        out_file_patterns => [],
        out_file_types => [],
        out_lines_after_patterns => [],
        out_lines_before_patterns => [],
        paths => [],
        print_results => 1,
        print_usage => 0,
        print_version => 0,
        recursive => 1,
        search_archives => 0,
        search_patterns => [],
        text_file_encoding => 'UTF-8',
        unique_lines => 0,
        verbose => 0,
    };
    bless $self, $class;
    return $self;
}

sub bool_to_string {
    my ($self, $bool) = @_;
    return $bool ? 'true' : 'false';
}

sub aref_to_string {
    my ($self, $aref) = @_;
    my $s = '[';
    if (@$aref) {
        foreach my $i (0..$#{$aref}) {
            if ($i > 0) {
                $s .= ', ';
            }
            $s .= '"' . $aref->[$i] . '"';
        }
    }
    $s .= ']';
    return $s;
}

sub set_property {
    my ($self, $name, $val) = @_;
    $self->{$name} = $val;
    if ($val == 1) {
        if ($name eq 'archives_only') {
            $self->{search_archives} = 1;
        } elsif ($name eq 'debug') {
            $self->{verbose} = 1;
        }
    }
}

sub add_exts {
    my ($self, $exts, $extaref) = @_;
    my $xs = [];
    if (ref($exts) eq 'ARRAY') {
        $xs = $exts;
    } else { # treat as a string
        my @split = split(',', $exts);
        $xs = \@split;
    }
    foreach my $x (@{$xs}) {
        push (@{$extaref}, $x);
    }
}

sub add_file_types {
    my ($self, $file_types, $ftaref) = @_;
    my $fts = [];
    if (ref($file_types) eq 'ARRAY') {
        $fts = $file_types;
    } else { # treat as a string
        my @split = split(',', $file_types);
        $fts = \@split;
    }
    foreach my $ft (@{$fts}) {
        push (@{$ftaref}, plsearch::FileTypes::from_name($ft));
    }
}

sub add_patterns {
    my ($self, $pats, $pataref) = @_;
    if (ref($pats) eq 'ARRAY') {
        foreach my $p (@{$pats}) {
            push (@{$pataref}, $p);
        }
    } else { # treat as a string
        push (@{$pataref}, $pats);
    }
}

sub to_string {
    my $self = shift @_;
    my $s = "SearchSettings(";
    $s .= 'archives_only=' . $self->bool_to_string($self->{archives_only});
    $s .= ', colorize=' . $self->bool_to_string($self->{colorize});
    $s .= ', debug=' . $self->bool_to_string($self->{debug});
    $s .= ', exclude_hidden=' . $self->bool_to_string($self->{exclude_hidden});
    $s .= ', first_match=' . $self->bool_to_string($self->{first_match});
    $s .= ', in_archive_extensions=' . $self->aref_to_string($self->{in_archive_extensions});
    $s .= ', in_archive_file_patterns=' . $self->aref_to_string($self->{in_archive_file_patterns});
    $s .= ', in_dir_patterns=' . $self->aref_to_string($self->{in_dir_patterns});
    $s .= ', in_extensions=' . $self->aref_to_string($self->{in_extensions});
    $s .= ', in_file_patterns=' . $self->aref_to_string($self->{in_file_patterns});
    $s .= ', in_file_types=' . $self->aref_to_string($self->{in_file_types});
    $s .= ', in_lines_after_patterns=' . $self->aref_to_string($self->{in_lines_after_patterns});
    $s .= ', in_lines_before_patterns=' . $self->aref_to_string($self->{in_lines_before_patterns});
    $s .= ', lines_after=' . $self->{lines_after};
    $s .= ', lines_after_to_patterns=' . $self->aref_to_string($self->{lines_after_to_patterns});
    $s .= ', lines_after_until_patterns=' . $self->aref_to_string($self->{lines_after_until_patterns});
    $s .= ', lines_before=' . $self->{lines_before};
    $s .= ', list_dirs=' . $self->bool_to_string($self->{list_dirs});
    $s .= ', list_files=' . $self->bool_to_string($self->{list_files});
    $s .= ', list_lines=' . $self->bool_to_string($self->{list_lines});
    $s .= ', max_line_length=' . $self->{max_line_length};
    $s .= ', multi_line_search=' . $self->bool_to_string($self->{multi_line_search});
    $s .= ', out_archive_extensions=' . $self->aref_to_string($self->{out_archive_extensions});
    $s .= ', out_archive_file_patterns=' . $self->aref_to_string($self->{out_archive_file_patterns});
    $s .= ', out_dir_patterns=' . $self->aref_to_string($self->{out_dir_patterns});
    $s .= ', out_extensions=' . $self->aref_to_string($self->{out_extensions});
    $s .= ', out_file_patterns=' . $self->aref_to_string($self->{out_file_patterns});
    $s .= ', out_file_types=' . $self->aref_to_string($self->{out_file_types});
    $s .= ', out_lines_after_patterns=' . $self->aref_to_string($self->{out_lines_after_patterns});
    $s .= ', out_lines_before_patterns=' . $self->aref_to_string($self->{out_lines_before_patterns});
    $s .= ', paths=' . $self->aref_to_string($self->{paths});
    $s .= ', print_results=' . $self->bool_to_string($self->{print_results});
    $s .= ', print_usage=' . $self->bool_to_string($self->{print_usage});
    $s .= ', print_version=' . $self->bool_to_string($self->{print_version});
    $s .= ', recursive=' . $self->bool_to_string($self->{recursive});
    $s .= ', search_archives=' . $self->bool_to_string($self->{search_archives});
    $s .= ', search_patterns=' . $self->aref_to_string($self->{search_patterns});
    $s .= ', text_file_encoding="' . $self->{text_file_encoding} . '"';
    $s .= ', unique_lines=' . $self->bool_to_string($self->{unique_lines});
    $s .= ', verbose=' . $self->bool_to_string($self->{verbose});
    $s .= ')';
    return $s;
}

1;

__END__
