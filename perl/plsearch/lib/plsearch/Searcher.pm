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

use plsearch::common;
use plsearch::FileType;
use plsearch::FileTypes;
use plsearch::FileUtil;
use plsearch::SearchResult;
use plsearch::SearchResultFormatter;

sub new {
    my $class = shift;
    my $self = {
        settings => shift,
        file_types => new plsearch::FileTypes(),
        results => [],
    };
    bless $self, $class;
    my $errs = $self->validate_settings();
    return ($self, $errs);
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

sub is_search_dir {
    my ($self, $d) = @_;
    if (plsearch::FileUtil::is_dot_dir($d)) {
        return 1;
    }
    my @path_elems = grep {$_ ne ''} File::Spec->splitdir($d);
    if ($self->{settings}->{exclude_hidden}) {
        foreach my $p (@path_elems) {
            if (plsearch::FileUtil::is_hidden($p)) {
                return 0;
            }
        }
    }
    if (scalar @{$self->{settings}->{in_dir_patterns}} &&
        !$self->any_matches_any_pattern(\@path_elems, $self->{settings}->{in_dir_patterns})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_dir_patterns}} &&
        $self->any_matches_any_pattern(\@path_elems, $self->{settings}->{out_dir_patterns})) {
        return 0;
    }
    return 1;
}

sub is_search_file {
    my ($self, $f) = @_;
    my $ext = plsearch::FileUtil::get_extension($f);
    if (scalar @{$self->{settings}->{in_extensions}} &&
        !(grep {$_ eq $ext} @{$self->{settings}->{in_extensions}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_extensions}} &&
        (grep {$_ eq $ext} @{$self->{settings}->{out_extensions}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{in_file_patterns}} &&
        !$self->matches_any_pattern($f, $self->{settings}->{in_file_patterns})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_file_patterns}} &&
        $self->matches_any_pattern($f, $self->{settings}->{out_file_patterns})) {
        return 0;
    }
    my $type = $self->{file_types}->get_file_type($f);
    if (scalar @{$self->{settings}->{in_file_types}} &&
        !(grep {$_ eq $type} @{$self->{settings}->{in_file_types}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_file_types}} &&
        (grep {$_ eq $type} @{$self->{settings}->{out_file_types}})) {
        return 0;
    }
    return 1;
}

sub is_archive_search_file {
    my ($self, $f) = @_;
    my $ext = plsearch::FileUtil::get_extension($f);
    if (scalar @{$self->{settings}->{in_archive_extensions}} &&
        !(grep {$_ eq $ext} @{$self->{settings}->{in_archive_extensions}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_archive_extensions}} &&
        (grep {$_ eq $ext} @{$self->{settings}->{out_archive_extensions}})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{in_archive_file_patterns}} &&
        !$self->matches_any_pattern($f, $self->{settings}->{in_archive_file_patterns})) {
        return 0;
    }
    if (scalar @{$self->{settings}->{out_archive_file_patterns}} &&
        $self->matches_any_pattern($f, $self->{settings}->{out_archive_file_patterns})) {
        return 0;
    }
    return 1;
}

sub rec_get_search_dirs {
    my ($self, $d) = @_;
    my $search_dirs = [];
    opendir(DIR, $d) or die $!;
    while (my $f = readdir(DIR)) {
        my $subfile = File::Spec->join($d, $f);
        if (-d $subfile && !plsearch::FileUtil::is_dot_dir($f) && $self->is_search_dir($subfile)) {
            push(@{$search_dirs}, $subfile);
        }
    }
    closedir(DIR);
    foreach my $search_dir (@{$search_dirs}) {
        my @merged = (@{$search_dirs}, @{$self->rec_get_search_dirs($search_dir)});
        $search_dirs = \@merged;
    }
    return $search_dirs;
}

sub filter_file {
    my ($self, $f) = @_;
    if ($self->{settings}->{exclude_hidden} && plsearch::FileUtil::is_hidden(basename($f))) {
        return 0;
    }
    if ($self->{file_types}->is_archive($f)) {
        if ($self->{settings}->{search_archives} && $self->is_archive_search_file($f)) {
            return 1;
        }
        return 0;
    }
    return !$self->{settings}->{archives_only} && $self->is_search_file($f);
}

sub rec_get_search_files {
    # print "rec_get_search_files\n";
    my ($self, $d) = @_;
    # print "d: $d\n";
    my $search_dirs = [];
    my $search_files = [];
    opendir(DIR, $d) or die $!;
    while (my $f = readdir(DIR)) {
        my $sub_file = File::Spec->join($d, $f);
        if ($sub_file !~ m[/\.{1,2}$]) {
            if (-d $sub_file && $self->is_search_dir($f)) {
                # print "-d $sub_file\n";
                push(@{$search_dirs}, $sub_file);
            } elsif (-f $sub_file && $self->filter_file($f)) {
                # print "-f $sub_file\n";
                push(@{$search_files}, $sub_file);
            }
        }
    }
    closedir(DIR);
    foreach my $search_dir (@{$search_dirs}) {
        my $sub_search_files = $self->rec_get_search_files($search_dir);
        push(@{$search_files}, @{$sub_search_files});
    }

    return $search_files;
}

sub get_search_files {
    my $self = shift;
    my $search_files = [];

    foreach my $path (@{$self->{settings}->{paths}}) {
        if (-d $path) {
            push(@{$search_files}, @{$self->rec_get_search_files($path)});
        } elsif (-f $path) {
            if ($self->filter_file($path)) {
                push(@{$search_files}, $path);
            } else {
                plsearch::common::log("ERROR: Startpath does not match search settings");
            }
        }
    }

    return $search_files;
}

sub search {
    my $self = shift;
    my $search_files = $self->get_search_files();
    if ($self->{settings}->{verbose}) {
        my @sorted_files = sort @{$search_files};
        my $sorted_dir_hash = {};
        foreach my $f (@sorted_files) {
            $sorted_dir_hash->{dirname($f)} = 1;
        }
        my @sorted_dirs = sort (keys %{$sorted_dir_hash});
        plsearch::common::log(sprintf("\nDirectories to be searched (%d):", scalar @sorted_dirs));
        foreach my $d (@sorted_dirs) {
            plsearch::common::log($d);
        }
        plsearch::common::log(sprintf("\nFiles to be searched (%d):", scalar @sorted_files));
        foreach my $f (@sorted_files) {
            plsearch::common::log($f);
        }
        plsearch::common::log('');
    }

    # search the files
    foreach my $f (@{$search_files}) {
        $self->search_file($f);
    }
}

sub search_file {
    my ($self, $f) = @_;
    my $type = $self->{file_types}->get_file_type($f);
    if ($type eq plsearch::FileType->TEXT || $type eq plsearch::FileType->CODE || $type eq plsearch::FileType->XML) {
        $self->search_text_file($f);
    } elsif ($type eq plsearch::FileType->BINARY) {
        $self->search_binary_file($f);
    }
}

sub search_text_file {
    my ($self, $f) = @_;
    if ($self->{settings}->{debug}) {
        plsearch::common::log("Searching text file $f");
    }
    if ($self->{settings}->{multi_line_search}) {
        $self->search_text_file_contents($f);
    } else {
        $self->search_text_file_lines($f);
    }
}

sub search_text_file_contents {
    my ($self, $f) = @_;
    my $contents = plsearch::FileUtil::get_file_contents($f);
    my $results = $self->search_multiline_string($contents);
    foreach my $r (@{$results}) {
        $r->{file} = $f;
        $self->add_search_result($r);
    }
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
    my ($self, $f) = @_;
    my $lines = plsearch::FileUtil::get_file_lines($f);
    my $results = $self->search_lines($lines);
    foreach my $r (@{$results}) {
        $r->{file} = $f;
        $self->add_search_result($r);
    }
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
                while ($line =~ /$pattern/go) {
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
    my ($self, $f) = @_;
    if ($self->{settings}->{debug}) {
        plsearch::common::log("Searching binary file $f");
    }
    my $contents = plsearch::FileUtil::get_file_contents($f);
    my $results = $self->search_binary_string($contents);
    foreach my $r (@{$results}) {
        $r->{file} = $f;
        $self->add_search_result($r);
    }
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


sub add_search_result {
    my ($self, $r) = @_;
    push(@{$self->{results}}, $r);
}

sub sort_results ($$) {
    my $a = $_[0];
    my $b = $_[1];
    if ($a->{file} eq $b->{file}) {
        if ($a->{line_num} == $b->{line_num}) {
            $a->{match_start_index} <=> $b->{match_start_index}
        } else {
            $a->{line_num} <=> $b->{line_num}
        }
    } else {
        $a->{file} cmp $b->{file}
    }
}

sub print_results {
    my ($self) = @_;
    my $len = scalar @{$self->{results}};
    my @sorted = sort sort_results @{$self->{results}};
    my $formatter = new plsearch::SearchResultFormatter($self->{settings});

    plsearch::common::log("Search results ($len):");
    # foreach my $r (@{$self->{results}}) {
    foreach my $r (@sorted) {
        # plsearch::common::log($r->to_string());
        plsearch::common::log($formatter->format($r));
    }
}

sub get_matching_dirs {
    my ($self) = @_;
    my $dir_hash = {};
    foreach my $r (@{$self->{results}}) {
        my $d = dirname($r->{file});
        $dir_hash->{$d}++;
    }
    my @dirs = keys %{$dir_hash};
    @dirs = sort(@dirs);
    return \@dirs;
}

sub print_matching_dirs {
    my ($self) = @_;
    my $dirs = $self->get_matching_dirs();
    plsearch::common::log(sprintf("\nDirectories with matches (%d):", scalar @{$dirs}));
    foreach my $d (@{$dirs}) {
        plsearch::common::log($d);
    }
}

sub get_matching_files {
    my ($self) = @_;
    my $file_hash = {};
    foreach my $r (@{$self->{results}}) {
        my $f = $r->{file};
        $file_hash->{$f}++;
    }
    my @files = keys %{$file_hash};
    @files = sort(@files);
    return \@files;
}

sub print_matching_files {
    my ($self) = @_;
    my $files = $self->get_matching_files();
    plsearch::common::log(sprintf("\nFiles with matches (%d):", scalar @{$files}));
    foreach my $f (@{$files}) {
        plsearch::common::log($f);
    }
}

sub get_matching_lines {
    my ($self) = @_;
    my $line_hash = {};
    my @lines;
    foreach my $r (@{$self->{results}}) {
        my $l = plsearch::common::trim($r->{line});
        $line_hash->{$l}++;
        push(@lines, $l);
    }
    if ($self->{settings}->{unique_lines}) {
        @lines = keys %{$line_hash};
    }
    @lines = sort {uc($a) cmp uc($b)} @lines;
    return \@lines;
}

sub print_matching_lines {
    my ($self) = @_;
    my $lines = $self->get_matching_lines();
    my $msg = "\nLines with matches (%d):";
    if ($self->{settings}->{unique_lines}) {
        $msg = "\nUnique lines with matches (%d):";
    }
    plsearch::common::log(sprintf($msg, scalar @{$lines}));
    foreach my $l (@{$lines}) {
        plsearch::common::log($l);
    }
}

1;

__END__
