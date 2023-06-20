###############################################################################
#
# Searcher.pm
#
# The file searcher
#
###############################################################################

package plsearch::Searcher;

use strict;
use warnings;

use File::Spec;
use File::Basename;

use lib $ENV{XFIND_PATH} . '/perl/plfind/lib';

use plfind::FileType;
use plfind::FileUtil;
use plfind::Finder;

use plsearch::SearchResult;
use plsearch::SearchResultFormatter;

sub new {
    my $class = shift;
    my $self = {
        settings => shift,
        results => [],
    };
    my ($finder, $finder_errs) = plfind::Finder->new($self->{settings});
    if (scalar @$finder_errs) {
        return ($self, $finder_errs);
    }

    $self->{finder} = $finder;

    bless $self, $class;
    my $validation_errs = $self->validate_settings();
    return ($self, $validation_errs);
}

sub validate_settings {
    my $self = shift;
    my $errs = [];
    unless (scalar @{$self->{settings}->{paths}}) {
        push(@{$errs}, 'Startpath not defined');
        return $errs;
    }
    foreach my $path (@{$self->{settings}->{paths}}) {
        if (!defined($path) || length($path) == 0) {
            push(@{$errs}, 'Startpath not defined');
            return $errs;
        }
        unless (-e $path) {
            push(@{$errs}, 'Startpath not found');
            return $errs;
        }
        unless (-r $path) {
            push(@{$errs}, 'Startpath not readable');
            return $errs;
        }
    }
    unless (scalar @{$self->{settings}->{search_patterns}}) {
        push(@{$errs}, 'No search patterns defined');
        return $errs;
    }
    if ($self->{settings}->{lines_after} < 0) {
        push(@{$errs}, 'Invalid lines_after');
        return $errs;
    }
    if ($self->{settings}->{lines_before} < 0) {
        push(@{$errs}, 'Invalid lines_before');
        return $errs;
    }
    if ($self->{settings}->{max_line_length} < 0) {
        push(@{$errs}, 'Invalid max_line_length');
    }
    return $errs;
}

sub matches_any_pattern {
    my ($self, $s, $patterns) = @_;
    foreach my $pattern (@{$patterns}) {
        if ($s =~ /$pattern/) {
            return 1;
        }
    }
    return 0;
}

sub any_matches_any_pattern {
    my ($self, $slist, $patterns) = @_;
    foreach my $s (@{$slist}) {
        if ($self->matches_any_pattern($s, $patterns)) {
            return 1;
        }
    }
    return 0;
}

sub search {
    my $self = shift;
    my $file_results = $self->{finder}->find();

    if ($self->{settings}->{verbose}) {
        my @dirs = map {$_->{path}} @{$file_results};
        my $uniq_dirs = plfind::common::uniq(\@dirs);
        my @sorted_dirs = sort @{$uniq_dirs};
        plfind::common::log_msg(sprintf("\nDirectories to be searched (%d):", scalar @sorted_dirs));
        foreach my $d (@sorted_dirs) {
            plfind::common::log_msg($d);
        }
        my @file_paths = map {$_->to_string()} @{$file_results};
        plfind::common::log_msg(sprintf("\nFiles to be searched (%d):", scalar @file_paths));
        foreach my $f (@file_paths) {
            plfind::common::log_msg($f);
        }
        plfind::common::log_msg('');
    }

    # search the files
    my $results = [];
    foreach my $fr (@{$file_results}) {
        my $file_search_results = $self->search_file($fr);
        push(@$results, @$file_search_results);
    }

    return $results;
}

sub search_file {
    my ($self, $fr) = @_;
    my $type = $fr->{file_type};
    my $results = [];
    if ($type eq plfind::FileType->TEXT || $type eq plfind::FileType->CODE || $type eq plfind::FileType->XML) {
        $results = $self->search_text_file($fr);
    } elsif ($type eq plfind::FileType->BINARY) {
        $results = $self->search_binary_file($fr);
    }
    return $results;
}

sub search_text_file {
    my ($self, $fr) = @_;
    if ($self->{settings}->{debug}) {
        plfind::common::log_msg("Searching text file " .  $fr->to_string());
    }
    if ($self->{settings}->{multi_line_search}) {
        return $self->search_text_file_contents($fr);
    } else {
        return $self->search_text_file_lines($fr);
    }
}

sub search_text_file_contents {
    my ($self, $fr) = @_;
    my $contents = plsearch::FileUtil::get_file_contents($fr->to_string());
    my $results = $self->search_multiline_string($contents);
    foreach my $r (@{$results}) {
        $r->{file} = $fr;
    }
    return $results;
}

sub get_new_line_indices {
    my ($self, $s) = @_;
    my @chars = split(//, $s);
    my $indices = [];
    my $i = 0;
    while ($i < scalar @chars) {
        my $c = $chars[$i];
        if ($c eq "\n") {
            push(@{$indices}, $i);
        }
        $i++;
    }
    return $indices;
}

sub print_array {
    my ($self, $aname, $aref) = @_;
    my $len = scalar @{$aref};
    print $aname . ' (' . $len . '): ';
    print "@{$aref}\n";
}

sub first_index {
    my ($self, $val, $aref) = @_;
    my $i = 0;
    my $len = scalar @{$aref};
    while ($i < $len) {
        if ($aref->[$i] == $val) {
            return $i;
        }
        $i++;
    }
    return -1;
}

sub get_before_indices {
    my ($self, $indices, $after_index) = @_;
    my @before_indices = grep { $_ <= $after_index } @{$indices};
    return \@before_indices;
}

sub get_after_indices {
    my ($self, $indices, $before_index) = @_;
    my @after_indices = grep { $_ > $before_index } @{$indices};
    return \@after_indices;
}

sub get_lines_before {
    my ($self, $s, $before_start_indices, $start_line_indices, $end_line_indices) = @_;
    my $lines_before = [];
    my $i = 0;
    while ($i < scalar @{$before_start_indices} && $i < $self->{settings}->{lines_before}) {
        my $start_index = $before_start_indices->[$i];
        my $end_index = $end_line_indices->[$self->first_index($start_index, $start_line_indices)];
        my $line = substr($s, $start_index, $end_index - $start_index);
        push(@{$lines_before}, $line);
        $i++;
    }
    return $lines_before;
}

sub get_lines_after {
    my ($self, $s, $after_start_indices, $start_line_indices, $end_line_indices) = @_;
    my $lines_after = [];
    my $i = 0;
    while ($i < scalar @{$after_start_indices} && $i < $self->{settings}->{lines_after}) {
        my $start_index = $after_start_indices->[$i];
        my $end_index = $end_line_indices->[$self->first_index($start_index, $start_line_indices)];
        my $line = substr($s, $start_index, $end_index - $start_index);
        push(@{$lines_after}, $line);
        $i++;
    }
    return $lines_after;
}

sub search_multiline_string {
    my ($self, $s) = @_;
    my $results = [];
    my $new_line_indices = $self->get_new_line_indices($s);
    #$self->print_array('new_line_indices', $new_line_indices);
    my @start_line_indices = map { $_ + 1 } @{$new_line_indices};
    unshift(@start_line_indices, 0);
    #$self->print_array('start_line_indices', \@start_line_indices);
    my @end_line_indices = (@{$new_line_indices}, (length($s)-1));
    #$self->print_array('end_line_indices', \@end_line_indices);
    foreach my $pattern (@{$self->{settings}->{search_patterns}}) {
        while ($s =~ /$pattern/go) {
            my $start_index = $-[0];
            my $end_index = $+[0];
            #print "match from $start_index to $end_index\n";
            my @before_start_indices = grep { $_ <= $start_index } @start_line_indices;
            #$self->print_array('before_start_indices', \@before_start_indices);
            my $m_line_start_index = 0;
            my $before_line_count = 0;
            if (@before_start_indices) {
                $before_line_count = (scalar @before_start_indices) - 1;
                $m_line_start_index = $before_start_indices[$before_line_count];
            }
            #print "before_line_count: $before_line_count\n";
            #print "m_line_start_index: $m_line_start_index\n";
            my $m_line_end_index = $end_line_indices[$self->first_index($m_line_start_index, \@start_line_indices)];
            #print "m_line_end_index: $m_line_end_index\n";
            my $line = substr($s, $m_line_start_index, $m_line_end_index - $m_line_start_index);
            my $lines_before = [];
            if ($self->{settings}->{lines_before}) {
                my $before_first_index = ($self->{settings}->{lines_before} + 1) * -1;
                my @before_indices = @before_start_indices[$before_first_index .. -1];
                #$self->print_array('before_indices', \@before_indices);
                $lines_before = $self->get_lines_before(
                    $s,
                    \@before_indices,
                    \@start_line_indices,
                    \@end_line_indices);
            }
            #print "start_index: $start_index\n";
            my $lines_after = [];
            if ($self->{settings}->{lines_after}) {
                my $after_start_indices = $self->get_after_indices(\@start_line_indices, $start_index);
                my @after_indices = @{$after_start_indices}[0 .. $self->{settings}->{lines_after}];
                #$self->print_array('after_indices', \@after_indices);
                $lines_after = $self->get_lines_after(
                    $s,
                    \@after_indices,
                    \@start_line_indices,
                    \@end_line_indices);
            }
            if (($lines_before && !$self->lines_before_match($lines_before))
                ||
                ($lines_after && !$self->lines_after_match($lines_after))) {
                next;
            }
            my $r = new plsearch::SearchResult(
                $pattern,
                '',
                $before_line_count + 1,
                $start_index - $m_line_start_index + 1,
                $end_index - $m_line_start_index + 1,
                $line,
                $lines_before,
                $lines_after);
            push(@{$results}, $r);
            if ($self->{settings}->{first_match}) {
                last;
            }
        }
    }
    return $results;
}

sub search_text_file_lines {
    my ($self, $fr) = @_;
    my $lines = plfind::FileUtil::get_file_lines($fr->to_string());
    my $results = $self->search_lines($lines);
    foreach my $r (@{$results}) {
        $r->{file} = $fr;
    }
    return $results;
}

sub lines_match {
    my ($self, $lines, $in_patterns, $out_patterns) = @_;
    if ((scalar @{$in_patterns} == 0 || $self->any_matches_any_pattern($lines, $in_patterns))
        &&
        (scalar @{$out_patterns} == 0 || !$self->any_matches_any_pattern($lines, $out_patterns))) {
        return 1;
    }
    return 0;
}

sub lines_before_match {
    my ($self, $lines_before) = @_;
    return $self->lines_match($lines_before, $self->{settings}->{in_lines_before_patterns},
        $self->{settings}->{out_lines_before_patterns});
}

sub lines_after_match {
    my ($self, $lines_after) = @_;
    return $self->lines_match($lines_after, $self->{settings}->{in_lines_after_patterns},
        $self->{settings}->{out_lines_after_patterns});
}

sub search_lines {
    my ($self, $lines) = @_;
    my $line_num = 0;
    my $line = '';
    my $lines_before = [];
    my $lines_after = [];
    my $pattern_match_hash = {};
    my $results = [];
    my $search = 1;
    SEARCHLINES:
    while ($search) {
        if (scalar @{$lines_after} > 0) {
            $line = shift(@{$lines_after});
        } else {
            $line = shift(@{$lines});
        }
        if (!$line) {
            last SEARCHLINES;
        }
        $line_num++;
        if ($self->{settings}->{lines_after}) {
            while(scalar @{$lines_after} < $self->{settings}->{lines_after}) {
                my $line_after = shift(@{$lines});
                if (!$line_after) {
                    last SEARCHLINES;
                } else {
                    push(@{$lines_after}, $line_after);
                }
            }
        }
        foreach my $pattern (@{$self->{settings}->{search_patterns}}) {
            if ($self->{settings}->{first_match} && $pattern_match_hash->{$pattern}) {
                next;
            }
            if ($line =~ /$pattern/) {
                if ((scalar @{$lines_before} > 0 && !$self->lines_before_match($lines_before))
                    ||
                    (scalar @{$lines_after} > 0  && !$self->lines_after_match($lines_after))) {
                    next;
                }
                my $keep_matching = 1;
                while ($keep_matching && $line =~ /$pattern/go) {
                    my $start_index = $-[0];
                    my $end_index = $+[0];
                    # make copies of lines_before and lines_after
                    my @lb = (@{$lines_before});
                    my @la = (@{$lines_after});
                    my $r = new plsearch::SearchResult(
                        $pattern,
                        '',
                        $line_num,
                        $start_index + 1,
                        $end_index + 1,
                        $line,
                        \@lb,
                        \@la);
                    push(@{$results}, $r);
                    $pattern_match_hash->{$pattern} = 1;
                    if ($self->{settings}->{first_match}) {
                        $keep_matching = 0;
                    }
                }
            }
        }
        if ($self->{settings}->{lines_before}) {
            if (scalar @{$lines_before} == $self->{settings}->{lines_before}) {
                shift(@{$lines_before});
            }
            if (scalar @{$lines_before} < $self->{settings}->{lines_before}) {
                push(@{$lines_before}, $line);
            }
        }

        if (scalar @{$lines} == 0) {
            last;
            $search = 0;
        }
    }
    return $results;
}

sub search_binary_file {
    my ($self, $fr) = @_;
    if ($self->{settings}->{debug}) {
        plfind::common::log_msg("Searching binary file " . $fr->to_string());
    }
    my $contents = plfind::FileUtil::get_file_contents($fr->to_string());
    my $results = $self->search_binary_string($contents);
    foreach my $r (@{$results}) {
        $r->{file} = $fr;
    }
    return $results;
}

sub search_binary_string {
    my ($self, $s) = @_;
    my $results = [];
    foreach my $pattern (@{$self->{settings}->{search_patterns}}) {
        if ($s =~ /$pattern/s) {
            while ($s =~ /$pattern/go) {
                my $start_index = $-[0];
                my $end_index = $+[0];
                my $r = new plsearch::SearchResult(
                    $pattern,
                    '',
                    0,
                    $start_index + 1,
                    $end_index + 1,
                    '',
                    [],
                    []);
                push(@{$results}, $r);
                if ($self->{settings}->{first_match}) {
                    last;
                }
            }
        }
    }
    return $results;
}

1;

__END__
