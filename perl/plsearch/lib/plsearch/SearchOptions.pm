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

# use Data::Dumper;
use DateTime::Format::DateParse;
use JSON::PP qw(decode_json);
use Path::Class;

use lib $ENV{'XFIND_PATH'} . '/perl/plfind/lib';

use plfind::ArgToken;
use plfind::ArgTokenType;
use plfind::ArgTokenizer;
use plfind::FileType;
use plfind::FileTypes;
use plfind::FileUtil;

use plsearch::config qw($SEARCH_OPTIONS_PATH);
use plsearch::SearchOption;
use plsearch::SearchSettings;

my $bool_action_hash = {
    'allmatches' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('first_match', !$bool);
    },
    'archivesonly' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('archives_only', $bool);
    },
    'colorize' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('colorize', $bool);
    },
    'debug' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('debug', $bool);
    },
    'excludehidden' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('include_hidden', !$bool);
    },
    'firstmatch' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('first_match', $bool);
    },
    'followsymlinks' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('follow_symlinks', $bool);
    },
    'help' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_usage', $bool);
    },
    'includehidden' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('include_hidden', $bool);
    },
    'multilinesearch' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('multi_line_search', $bool);
    },
    'nocolorize' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('colorize', !$bool);
    },
    'nofollowsymlinks' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('follow_symlinks', !$bool);
    },
    'noprintmatches' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_results', !$bool);
    },
    'norecursive' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('recursive', !$bool);
    },
    'nosearcharchives' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('search_archives', !$bool);
    },
    'printdirs' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_dirs', $bool);
    },
    'printfiles' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_files', $bool);
    },
    'printlines' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_lines', $bool);
    },
    'printmatches' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_results', $bool);
    },
    'recursive' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('recursive', $bool);
    },
    'searcharchives' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('search_archives', $bool);
    },
    'sort-ascending' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('sort_descending', !$bool);
    },
    'sort-caseinsensitive' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('sort_case_insensitive', $bool);
    },
    'sort-casesensitive' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('sort_case_insensitive', !$bool);
    },
    'sort-descending' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('sort_descending', $bool);
    },
    'uniquelines' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('unique_lines', $bool);
    },
    'verbose' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('verbose', $bool);
    },
    'version' => sub {
        my ($bool, $settings) = @_;
        $settings->set_property('print_version', $bool);
    }
};

my $str_action_hash = {
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
    'linesaftertopattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{lines_aftertopatterns});
    },
    'linesafteruntilpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{lines_afteruntilpatterns});
    },
    'maxlastmod' => sub {
        my ($s, $settings) = @_;
        $settings->{max_last_mod} = DateTime::Format::DateParse->parse_datetime($s);
    },
    'minlastmod' => sub {
        my ($s, $settings) = @_;
        $settings->{min_last_mod} = DateTime::Format::DateParse->parse_datetime($s);
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
        $settings->add_paths($s);
    },
    'searchpattern' => sub {
        my ($s, $settings) = @_;
        $settings->add_patterns($s, $settings->{search_patterns});
    },
    'sort-by' => sub {
        my ($s, $settings) = @_;
        $settings->set_sort_by($s);
    },
};

my $int_action_hash = {
    'linesafter' => sub {
        # my ($s, $settings) = @_;
        # $settings->{lines_after} = int($s);
        my ($i, $settings) = @_;
        $settings->{lines_after} = $i;
    },
    'linesbefore' => sub {
        my ($i, $settings) = @_;
        $settings->{lines_before} = $i;
    },
    'maxdepth' => sub {
        my ($i, $settings) = @_;
        $settings->{max_depth} = $i;
    },
    'maxlinelength' => sub {
        my ($i, $settings) = @_;
        $settings->{max_line_length} = $i;
    },
    'maxsize' => sub {
        my ($i, $settings) = @_;
        $settings->{max_size} = $i;
    },
    'mindepth' => sub {
        my ($i, $settings) = @_;
        $settings->{min_depth} = $i;
    },
    'minsize' => sub {
        my ($i, $settings) = @_;
        $settings->{min_size} = $i;
    },
};

sub new {
    my $class = shift;
    my $options_hash = set_options_from_json();
    my $arg_tokenizer = plfind::ArgTokenizer->new($options_hash);
    my $self = {
        options => $options_hash,
        arg_tokenizer => $arg_tokenizer,
    };
    bless $self, $class;
    return $self;
}

sub set_options_from_json {
    my $options_hash = {};
    my $contents = $SEARCH_OPTIONS_PATH->slurp;
    my $options_json_hash = decode_json $contents;
    foreach my $search_option (@{$options_json_hash->{searchoptions}}) {
        my $short = $search_option->{short};
        my $long = $search_option->{long};
        my $desc = $search_option->{desc};
        my $arg_type = plfind::ArgTokenType->UNKNOWN;
        if (exists $bool_action_hash->{$long}) {
            $arg_type = plfind::ArgTokenType->BOOL;
        } elsif (exists $str_action_hash->{$long}) {
            $arg_type = plfind::ArgTokenType->STR;
        } elsif (exists $int_action_hash->{$long}) {
            $arg_type = plfind::ArgTokenType->INT;
        }
        $options_hash->{$long} = plsearch::SearchOption->new($short, $long, $desc, $arg_type);
        if (defined $short) {
            $options_hash->{$short} = $options_hash->{$long};
        }
    }
    return $options_hash;
}

sub update_settings_from_arg_tokens {
    my ($self, $settings, $arg_tokens) = @_;
    my @errs;
    foreach my $arg_token (@$arg_tokens) {
        if (scalar @errs) {
            return \@errs;
        }
        my $arg_value = $arg_token->{value};
        if ($arg_token->{type} eq plfind::ArgTokenType->BOOL) {
            if (plfind::common::is_bool($arg_value)) {
                &{$bool_action_hash->{$arg_token->{name}}}($arg_value, $settings);
            } else {
                push(@errs, 'Invalid value for option: ' . $arg_token->{name});
            }
        } elsif ($arg_token->{type} eq plfind::ArgTokenType->STR) {
            if ($arg_token->{name} eq 'settings-file') {
                my $file_path = file($arg_value);
                my $settings_file_errors = $self->update_settings_from_file($settings, $file_path);
                push(@errs, @$settings_file_errors);
            } elsif (ref $arg_value eq 'ARRAY') {
                foreach my $val (@$arg_value) {
                    &{$str_action_hash->{$arg_token->{name}}}($val, $settings);
                }
            } else {
                # assume scalar
                &{$str_action_hash->{$arg_token->{name}}}($arg_value, $settings);
            }
        } elsif ($arg_token->{type} eq plfind::ArgTokenType->INT) {
            if ($arg_value =~ /^\d+$/) {
                &{$int_action_hash->{$arg_token->{name}}}($arg_value, $settings);
            } else {
                push(@errs, 'Invalid value for option: ' . $arg_token->{name});
            }
        } else {
            # should never reach here
            push(@errs, 'Invalid option: ' . $arg_token->{name});
        }
    }
    return \@errs;
}

sub update_settings_from_json {
    my ($self, $settings, $json) = @_;
    my ($arg_tokens, $errs) = $self->{arg_tokenizer}->tokenize_json($json);
    if (scalar @$errs) {
        return $errs;
    }
    return $self->update_settings_from_arg_tokens($settings, $arg_tokens);
}

sub settings_from_json {
    # $json is a string with json contents
    my ($self, $json) = @_;
    my $settings = plsearch::SearchSettings->new();
    my $errs = $self->update_settings_from_json($settings, $json);
    return ($settings, $errs);
}

sub update_settings_from_file {
    # $file_path is instance of Path::Class::File
    my ($self, $settings, $file_path) = @_;
    my ($arg_tokens, $errs) = $self->{arg_tokenizer}->tokenize_file($file_path);
    if (scalar @$errs) {
        return $errs;
    }
    return $self->update_settings_from_arg_tokens($settings, $arg_tokens);
}

sub settings_from_file {
    # $file_path is instance of Path::Class::File
    my ($self, $file_path) = @_;
    my $settings = plsearch::SearchSettings->new();
    my $errs = $self->update_settings_from_file($settings, $file_path);
    return ($settings, $errs);
}

sub update_settings_from_args {
    my ($self, $settings, $args) = @_;
    my ($arg_tokens, $errs) = $self->{arg_tokenizer}->tokenize_args($args);
    if (scalar @$errs) {
        return $errs;
    }
    return $self->update_settings_from_arg_tokens($settings, $arg_tokens);
}

sub settings_from_args {
    my ($self, $args) = @_;
    my $settings = plsearch::SearchSettings->new();
    # default print_results to true since running as cli
    $settings->set_property('print_results', 1);
    my $errs = $self->update_settings_from_args($settings, $args);
    return ($settings, $errs);
}

sub get_usage_string {
    my $self = shift;
    my $usage = "Usage:\n plsearch [options] -s <searchpattern> <path> [<path> ...]\n\nOptions:\n";
    my $longest = 0;
    my $options_with_sort_key = {};
    foreach my $opt_key (keys %{$self->{options}}) {
        my $option = $self->{options}->{$opt_key};
        my $long_arg = $option->{long_arg};
        my $short_arg = $option->{short_arg};
        my $sort_key = $long_arg;
        if (defined $short_arg) {
            $sort_key = lc($short_arg) . 'a' . $long_arg;
        }
        $options_with_sort_key->{$sort_key} = $option;
    }
    my @sort_keys = keys %{$options_with_sort_key};
    @sort_keys = sort {$plsearch::SearchOptions::a cmp $plsearch::SearchOptions::b} @sort_keys;
    my $opt_strs_with_key = {};
    my $opt_descs_with_key = {};
    foreach my $sort_key (@sort_keys) {
        my $option = $options_with_sort_key->{$sort_key};
        my $opt_str = '';
        if ($option->{short_arg}) {
            $opt_str = '-' . $option->{short_arg} . ',';
        }
        $opt_str .= '--' . $option->{long_arg};
        if (length($opt_str) > $longest) {
            $longest = length($opt_str);
        }
        $opt_strs_with_key->{$sort_key} = $opt_str;
        $opt_descs_with_key->{$sort_key} = $option->{desc};
    }
    my $format_str = " %-" . $longest . "s  %s\n";
    foreach my $sort_key (@sort_keys) {
       $usage .= sprintf($format_str, $opt_strs_with_key->{$sort_key},
        $opt_descs_with_key->{$sort_key});
    }
    return $usage;
}

sub usage {
    my $self = shift;
    print $self->get_usage_string();
}

1;

__END__
