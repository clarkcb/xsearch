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

use plfind::ConsoleColor;
use plfind::common;
use plfind::FileResultFormatter;

sub new {
    my $class = shift;
    my $settings = shift;
    my $self = {
        settings       => $settings,
        file_formatter => plfind::FileResultFormatter->new($settings)
    };
    bless $self, $class;
    if ($self->{settings}->{colorize}) {
        $self->{format_line} = sub { my ($_self, $line) = @_; return $_self->format_line_with_color($line) };
        $self->{format_match} = sub { my ($_self, $match) = @_; return $_self->format_match_with_color($match) };
    } else {
        $self->{format_line} = sub { my ($_self, $line) = @_; return $line };
        $self->{format_match} = sub { my ($_self, $match) = @_; return $match };
    }
    return $self;
}

sub format_line_with_color {
    my ($self, $line) = @_;
    my $formatted_line = $line;
    foreach my $p (@{$self->{settings}->{search_patterns}}) {
        if ($formatted_line =~ /$p/go) {
            my $start_index = $-[0];
            my $end_index = $+[0];
            $formatted_line = colorize($formatted_line, $start_index, $end_index, $self->{settings}->{line_color});
            last;
        }
    }
    return $formatted_line;
}

sub format_match_with_color {
    my ($self, $match) = @_;
    return colorize($match, 0, length($match), $self->{settings}->{line_color});
}

sub format_line {
    my ($self, $line) = @_;
    $self->{format_line}->($self, $line);
}

sub format_match {
    my ($self, $match) = @_;
    $self->{format_match}->($self, $match);
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
    my $s = $self->{file_formatter}->format_file_result($result->{file});
    if ($result->{line_num}) {
        $s .= ': ' . $result->{line_num} . ': ';
        $s .= '[' . $result->{match_start_index} . ':' . $result->{match_end_index};
        $s .= ']: ' . $self->format_result_line($result);
    } else {
        $s .= ' matches at ';
        $s .= '[' . $result->{match_start_index} . ':' . $result->{match_end_index} . ']';
    }
    return $s;
}

sub format_result_match {
    my ($self, $result) = @_;

    if (!$result->{line}) {
        return '';
    }

    my $match_start_idx = $result->{match_start_index} - 1;
    my $match_end_idx = $result->{match_end_index} - 1;
    my $match_length = $result->{match_end_index} - $result->{match_start_index};

    my $prefix = '';
    my $suffix = '';
    my $color_start_idx = 0;
    my $color_end_idx = $match_length;

    if ($match_length > $self->{settings}->{max_line_length}) {
        if ($match_start_idx > 2) {
            $prefix = '...';
        }
        $suffix = '...';
        $color_start_idx = length($prefix);
        $color_end_idx = $self->{settings}->{max_line_length} - length($suffix);
        $match_end_idx = $match_start_idx + $color_end_idx;
        $match_start_idx = $match_start_idx + $color_start_idx;
    }

    my $match_string = $prefix . substr($result->{line}, $match_start_idx, $match_end_idx - $match_start_idx) . $suffix;
    if ($self->{settings}->{colorize}) {
        $match_string = colorize($match_string, $color_start_idx, $color_end_idx, $self->{settings}->{line_color});
    }
    return $match_string;
}

sub format_result_line {
    my ($self, $result) = @_;

    if (!$result->{line} || $self->{settings}->{max_line_length} == 0) {
        return '';
    }

    my $max_limit = $self->{settings}->{max_line_length} > 0;

    if ($max_limit && ($result->{match_end_index} - $result->{match_start_index}) > $self->{settings}->{max_line_length}) {
        return $self->format_result_match($result);
    }

    my $result_line = $result->{line};
    my $result_line_length = length($result_line);
    my $line_start_idx = plfind::common::leading_whitespace_chars($result_line);
    my $line_end_idx = $result_line_length - $line_start_idx - plfind::common::trailing_whitespace_chars($result_line) - 1;

    my $match_length = $result->{match_end_index} - $result->{match_start_index};
    my $match_start_idx = $result->{match_start_index} - 1 - $line_start_idx;
    my $match_end_idx = $match_start_idx + $match_length;

    my $trimmed_length = $line_end_idx - $line_start_idx;

    if ($trimmed_length == 0) {
        return '';
    }

    my $prefix = '';
    my $suffix = '';

    if ($max_limit && $trimmed_length > $self->{settings}->{max_line_length}) {
        $line_start_idx = $result->{match_start_index} - 1;
        $line_end_idx = $line_start_idx + $match_length;
        $match_start_idx = 0;
        $match_end_idx = $match_length;

        my $current_len = $line_end_idx - $line_start_idx;
        while ($current_len < $self->{settings}->{max_line_length}) {
            if ($line_start_idx > 0) {
                $line_start_idx--;
                $match_start_idx++;
                $match_end_idx++;
                $current_len++;
            }
            if ($current_len < $self->{settings}->{max_line_length} && $line_end_idx < $trimmed_length) {
                $line_end_idx++;
                $current_len++;
            }
        }

        if ($line_start_idx > 2) {
            $prefix = '...';
            $line_start_idx += 3;
        }

        if ($line_end_idx < ($trimmed_length - 3)) {
            $suffix = '...';
            $line_end_idx -= 3;
        }
    } else {
        $line_end_idx++;
    }

    my $formatted = $prefix . substr($result_line, $line_start_idx, $line_end_idx) . $suffix;

    if ($self->{settings}->{colorize}) {
        $formatted = colorize($formatted, $match_start_idx, $match_end_idx, $self->{settings}->{line_color});
    }
    return $formatted;
}

sub colorize {
    my ($s, $match_start_index, $match_end_index, $color) = @_;
    plfind::FileResultFormatter::colorize($s, $match_start_index, $match_end_index, $color);
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
    my $file = $self->{file_formatter}->format_file_result($result->{file});
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
