###############################################################################
#
# SearchOptions.pm
#
# Helper class for search options
#
###############################################################################

package plsearch::SearchOptions;

use strict;
use warnings;

# use XML::Simple;
use Data::Dumper;
use JSON::PP qw(decode_json);

use plsearch::common;
use plsearch::config;
use plsearch::FileUtil;
use plsearch::SearchOption;
use plsearch::SearchSettings;

my $arg_action_hash = {
    'encoding' => sub {
        my ($s, $settings) = @_;
        $settings->{text_file_encoding} = $s;
    },
    'in-archiveext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{in_archive_extensions});
    },
    'in-archivefilepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_archive_file_patterns});
    },
    'in-dirpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_dir_patterns});
    },
    'in-ext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{in_extensions});
    },
    'in-filepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_file_patterns});
    },
    'in-filetype' => sub {
        my ($s, $settings) = @_;
        $settings->add_file_types($s, $settings->{in_file_types});
    },
    'in-linesafterpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_lines_after_patterns});
    },
    'in-linesbeforepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{in_lines_before_patterns});
    },
    'linesafter' => sub {
        my ($s, $settings) = @_;
        $settings->{lines_after} = int($s);
    },
    'linesaftertopattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{lines_aftertopatterns});
    },
    'linesafteruntilpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{lines_afteruntilpatterns});
    },
    'linesbefore' => sub {
        my ($s, $settings) = @_;
        $settings->{lines_before} = int($s);
    },
    'maxlinelength' => sub {
        my ($s, $settings) = @_;
        $settings->{max_line_length} = int($s);
    },
    'out-archiveext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{out_archive_extensions});
    },
    'out-archivefilepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_archive_file_patterns});
    },
    'out-dirpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_dir_patterns});
    },
    'out-ext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{out_extensions});
    },
    'out-filepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_file_patterns});
    },
    'out-filetype' => sub {
        my ($s, $settings) = @_;
        $settings->add_file_types($s, $settings->{out_file_types});
    },
    'out-linesafterpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_lines_after_patterns});
    },
    'out-linesbeforepattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{out_lines_before_patterns});
    },
    'path' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{paths}}, $s);
    },
    'searchpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{search_patterns});
    },
    'settings-file' => sub {
        my ($s, $settings) = @_;
        settings_from_file($s, $settings);
    }
};

my $bool_flag_action_hash = {
    'allmatches' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('first_match', !$b);
    },
    'archivesonly' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('archives_only', $b);
    },
    'colorize' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('colorize', $b);
    },
    'debug' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('debug', $b);
    },
    'excludehidden' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('exclude_hidden', $b);
    },
    'firstmatch' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('first_match', $b);
    },
    'help' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('print_usage', $b);
    },
    'includehidden' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('exclude_hidden', !$b);
    },
    'listdirs' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('list_dirs', $b);
    },
    'listfiles' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('list_files', $b);
    },
    'listlines' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('list_lines', $b);
    },
    'multilinesearch' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('multi_line_search', $b);
    },
    'nocolorize' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('colorize', !$b);
    },
    'noprintmatches' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('print_results', !$b);
    },
    'norecursive' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('recursive', !$b);
    },
    'nosearcharchives' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('search_archives', !$b);
    },
    'printmatches' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('print_results', $b);
    },
    'recursive' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('recursive', $b);
    },
    'searcharchives' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('search_archives', $b);
    },
    'uniquelines' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('unique_lines', $b);
    },
    'verbose' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('verbose', $b);
    },
    'version' => sub {
        my ($b, $settings) = @_;
        $settings->set_property('print_version', $b);
    }
};

sub new {
    my $class = shift;
    my $self = {
        options => set_options_from_json(),
    };
    bless $self, $class;
    return $self;
}

sub set_options_from_json {
    my $options_hash = {};
    my $options_json_hash = decode_json plsearch::FileUtil::get_file_contents($SEARCHOPTIONSPATH);
    foreach my $search_option (@{$options_json_hash->{searchoptions}}) {
        my $short = $search_option->{short};
        my $long = $search_option->{long};
        my $desc = $search_option->{desc};
        my $func = sub {};
        if (exists $arg_action_hash->{$long}) {
            $func = $arg_action_hash->{$long};
        } elsif (exists $bool_flag_action_hash->{$long}) {
            $func = $bool_flag_action_hash->{$long};
        }
        my $opt = new plsearch::SearchOption($short, $long, $desc, $func);
        $options_hash->{$long} = $opt;
        if ($short) {
            $options_hash->{$short} = $options_hash->{$long};
        }
    }
    return $options_hash;
}

sub settings_from_file {
    my ($file_path, $settings) = @_;
    my $errs = [];
    unless (-e $file_path) {
        push(@{$errs}, 'Settings file not found: ' . $file_path);
        return $errs;
    }
    my $json = plsearch::FileUtil::get_file_contents($file_path);
    return __from_json($json, $settings);
}

# private function
sub __from_json {
    my ($json, $settings) = @_;
    #print "\$json: '$json'\n";
    my $errs = [];
    my $json_hash = decode_json $json;
    my @opt_names = keys %{$json_hash};
    foreach my $o (@opt_names) {
        if (exists $arg_action_hash->{$o}) {
            &{$arg_action_hash->{$o}}($json_hash->{$o}, $settings);
        } elsif (exists $bool_flag_action_hash->{$o}) {
            &{$bool_flag_action_hash->{$o}}($json_hash->{$o}, $settings);
        } else {
            push(@{$errs}, 'Invalid option: ' . $o);
        }
    }
    return $errs;
}

# public method (made available for unit testing)
sub settings_from_json {
    my ($self, $json, $settings) = @_;
    #print "\$json: '$json'\n";
    __from_json($json, $settings);
}

sub settings_from_args {
    my ($self, $args) = @_;
    my $settings = new plsearch::SearchSettings();
    my @errs;
    while (scalar @{$args}) {
        my $arg = shift @{$args};
        if ($arg =~ /^\-+/) {
            $arg =~ s/^\-+//;
            if (exists $self->{options}->{$arg}) {
                my $opt = $self->{options}->{$arg};
                my $long = $opt->{longarg};
                if (exists $arg_action_hash->{$long}) {
                    if (scalar @{$args}) {
                        my $val = shift @{$args};
                        &{$arg_action_hash->{$long}}($val, $settings);
                    } else {
                        push(@errs, "Missing value for $arg");
                    }
                } elsif (exists $bool_flag_action_hash->{$long}) {
                    &{$bool_flag_action_hash->{$long}}(1, $settings);
                }
            } else {
                push(@errs, "Invalid option: $arg");
            }
        } else {
            push (@{$settings->{paths}}, $arg);
        }
    }
    return ($settings, \@errs);
}

sub usage {
    my $self = shift;
    print $self->get_usage_string();
}

sub get_usage_string {
    my $self = shift;
    my $usage = "Usage:\n plsearch [options] -s <searchpattern> <path> [<path> ...]\n\nOptions:\n";
    my $longest = 0;
    my $options_with_sortkey = {};
    my @opt_strs_with_descs;
    foreach my $opt_key (keys %{$self->{options}}) {
        my $option = $self->{options}->{$opt_key};
        my $long = $option->{longarg};
        my $short = $option->{shortarg};
        my $sortkey = $long;
        if ($short) {
            $sortkey = lc($short) . 'a' . $long;
        }
        $options_with_sortkey->{$sortkey} = $option;
    }
    my @sortkeys = keys %{$options_with_sortkey};
    @sortkeys = sort {$plsearch::SearchOptions::a cmp $plsearch::SearchOptions::b} @sortkeys;
    my $opt_strs_with_key = {};
    my $opt_descs_with_key = {};
    foreach my $sortkey (@sortkeys) {
        my $option = $options_with_sortkey->{$sortkey};
        my $opt_str = '';
        if ($option->{shortarg}) {
            $opt_str = '-' . $option->{shortarg} . ',';
        }
        $opt_str .= '--' . $option->{longarg};
        if (length($opt_str) > $longest) {
            $longest = length($opt_str);
        }
        $opt_strs_with_key->{$sortkey} = $opt_str;
        $opt_descs_with_key->{$sortkey} = $option->{desc};
    }
    my $format_str = " %-" . $longest . "s  %s\n";
    foreach my $sortkey (@sortkeys) {
       $usage .= sprintf($format_str, $opt_strs_with_key->{$sortkey},
        $opt_descs_with_key->{$sortkey});
    }
    return $usage;
}

1;

__END__
