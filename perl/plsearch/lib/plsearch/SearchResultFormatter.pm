###############################################################################
#
# SearchResultFormatter.pm
#
# Provides formatting of search result instances
#
###############################################################################

package plsearch::SearchResultFormatter;

use strict;
use warnings;

use lib $ENV{XFIND_PATH} . '/perl/plfind/lib';

use plfind::common;

use plsearch::Color;

sub new {
    my $class = shift;
    my $self = {
        settings => shift,
    };
    bless $self, $class;
    return $self;
}

sub format {
    my ($self, $result) = @_;
    if (scalar @{$result->{lines_before}} || scalar @{$result->{lines_after}}) {
        return $self->multi_line_format($result);
    }
    return $self->single_line_format($result);
}

sub single_line_format {
    my ($self, $result) = @_;
    my $s = $result->{file}->to_string();
    if ($result->{line_num}) {
        $s .= ': ' . $result->{line_num} . ': ';
        $s .= '[' . $result->{match_start_index} . ':' . $result->{match_end_index};
        $s .= ']: ' . $self->format_matching_line($result);
    } else {
        $s .= ' matches at ';
        $s .= '[' . $result->{match_start_index} . ':' . $result->{match_end_index} . ']';
    }
    return $s;
}

sub format_matching_line {
    my ($self, $result) = @_;
    my $formatted = $result->{line};
    # print "formatted: \"$formatted\"\n";
    my $leading_whitespace_count = plfind::common::leading_whitespace_chars($formatted);
    # remove leading and trailing whitespace
    $formatted =~ s/^\s+|\s+$//g;
    my $formatted_length = length($formatted);
    # my $line_start_index = 0;
    # my $line_end_index = $formatted_length - 1;
    my $max_line_end_index = $formatted_length - 1;
    my $match_length = $result->{match_end_index} - $result->{match_start_index};

    # track where match start and end indices end up (changing to zero-indexed)
    my $match_start_index = $result->{match_start_index} - 1 - $leading_whitespace_count;
    my $match_end_index = $match_start_index + $match_length;

    # if longer than max_line_length, walk out from matching indices
    if ($formatted_length > $self->{settings}->{max_line_length}) {
        my $line_start_index = $match_start_index;
        my $line_end_index = $line_start_index + $match_length;
        $match_start_index = 0;
        $match_end_index = $match_length;

        # adjust left if/until line_end_index < $formatted_length
        while ($line_end_index > $formatted_length - 1) {
            $line_start_index--;
            $line_end_index--;
            $match_start_index++;
            $match_end_index++;
        }

        $formatted_length = $line_end_index - $line_start_index;

        while ($formatted_length < $self->{settings}->{max_line_length}) {
            if ($line_start_index > 0) {
                $line_start_index--;
                $match_start_index++;
                $match_end_index++;
                $formatted_length = $line_end_index - $line_start_index;
            }
            if ($formatted_length < $self->{settings}->{max_line_length} && $line_end_index < $max_line_end_index) {
                $line_end_index++;
            }
            $formatted_length = $line_end_index - $line_start_index;
        }
        $formatted = substr($formatted, $line_start_index, $formatted_length);

        if ($line_start_index > 2) {
            $formatted = '...' . substr($formatted, 3);
        }
        if ($line_end_index < $max_line_end_index - 3) {
            $formatted = substr($formatted, 0, $formatted_length - 3) . '...';
        }
    }

    if ($self->{settings}->{colorize}) {
        $formatted = colorize($formatted, $match_start_index, $match_end_index);
    }
    return $formatted;
}

sub colorize {
    my ($s, $match_start_index, $match_end_index) = @_;
    my $match_length = $match_end_index - $match_start_index;
    my $c = substr($s, 0, $match_start_index);
    $c .= plsearch::Color->GREEN;
    $c .= substr($s, $match_start_index, $match_length);
    $c .= plsearch::Color->RESET;
    $c .= substr($s, $match_start_index + $match_length);
    return $c;
}

sub line_num_padding {
    my ($result) = @_;
    return length(sprintf("%d", $result->{line_num} + (scalar @{$result->{lines_after}})));
}

sub trim_newline {
    my ($s) = @_;
    $s =~ s/[\r\n]+$//;
    return $s;
}

sub multi_line_format {
    my ($self, $result) = @_;
    my $file = $result->{file}->to_string();
    my $s = ('=' x 80) . "\n$file: $result->{line_num}: ";
    $s .= "[$result->{match_start_index}:$result->{match_end_index}]\n";
    $s .= ('-' x 80) . "\n";
    my $lines_format = sprintf(" %%%dd | %%s\n", line_num_padding($result));
    my $current_line_num = $result->{line_num};
    if (scalar @{$result->{lines_before}}) {
        $current_line_num -= (scalar @{$result->{lines_before}});
        foreach my $line_before (@{$result->{lines_before}}) {
            $s .= sprintf(' '.$lines_format, $current_line_num, trim_newline($line_before));
            $current_line_num++;
        }
    }
    my $line = trim_newline($result->{line});
    if ($self->{settings}->{colorize}) {
        $line = colorize($line, $result->{match_start_index} - 1, $result->{match_end_index} - 1);
    }
    $s .= sprintf('>'.$lines_format, $current_line_num, $line);
    if (scalar @{$result->{lines_after}}) {
        $current_line_num++;
        foreach my $line_after (@{$result->{lines_after}}) {
            $s .= sprintf(' '.$lines_format, $current_line_num, trim_newline($line_after));
            $current_line_num++;
        }
    }
    return $s;
}

1;

__END__
