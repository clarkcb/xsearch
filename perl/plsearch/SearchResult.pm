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

sub new {
    my $class = shift;
    my $self = {
        pattern => shift,
        file => shift,
        linenum => shift,
        match_start_index => shift,
        match_end_index => shift,
        line => shift,
        lines_before => shift || [],
        lines_after => shift || [],
    };
    bless $self, $class;
    return $self;
}

sub to_string {
    my $self = shift;
    if (scalar @{$self->{lines_before}} || scalar @{$self->{lines_after}}) {
        return $self->multiline_tostring();
    }
    return $self->singleline_tostring();
}

sub singleline_tostring {
    my $self = shift;
    my $s = $self->{file};
    if ($self->{linenum}) {
        $s .= ': ' . $self->{linenum} . ': ';
        $s .= '[' . $self->{match_start_index} . ':' . $self->{match_end_index};
        $s .= ']: ' . plsearch::common::trim($self->{line});
    } else {
        $s .= ' matches';
    }
    return $s;
}

sub linenum_padding {
    my $self = shift;
    return length(sprintf("%d", $self->{linenum} + (scalar @{$self->{lines_after}})));
}

sub trim_newline {
    my ($self, $s) = @_;
    $s =~ s/[\r\n]+$//;
    return $s;
}

sub multiline_tostring {
    my $self = shift;
    my $s = ('=' x 80) . "\n$self->{file}: $self->{linenum}: ";
    $s .= "[$self->{match_start_index}:$self->{match_end_index}]\n";
    $s .= ('-' x 80) . "\n";
    my $lineformat = sprintf(" %%%dd | %%s\n", $self->linenum_padding());
    my $current_linenum = $self->{linenum};
    if (scalar @{$self->{lines_before}}) {
        $current_linenum -= (scalar @{$self->{lines_before}});
        foreach my $line_before (@{$self->{lines_before}}) {
            $s .= sprintf(' '.$lineformat, $current_linenum, $self->trim_newline($line_before));
            $current_linenum++;
        }
    }
    $s .= sprintf('>'.$lineformat, $current_linenum, $self->trim_newline($self->{line}));
    if (scalar @{$self->{lines_after}}) {
        $current_linenum++;
        foreach my $line_after (@{$self->{lines_after}}) {
            $s .= sprintf(' '.$lineformat, $current_linenum, $self->trim_newline($line_after));
            $current_linenum++;
        }
    }
    return $s;
}

1;

__END__
