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

use XML::Simple;
use Data::Dumper;
use plsearch::common;
use plsearch::config;
use plsearch::SearchOption;
use plsearch::SearchSettings;

my $arg_action_hash = {
    'in-archiveext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{in_archiveextensions});
    },
    'in-archivefilepattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{in_archivefilepatterns}}, $s);
    },
    'in-dirpattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{in_dirpatterns}}, $s);
    },
    'in-ext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{in_extensions});
    },
    'in-filepattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{in_filepatterns}}, $s);
    },
    'in-linesafterpattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{in_linesafterpatterns}}, $s);
    },
    'in-linesbeforepattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{in_linesbeforepatterns}}, $s);
    },
    'linesafter' => sub {
        my ($s, $settings) = @_;
        $settings->{linesafter} = int($s);
    },
    'linesaftertopattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{linesaftertopatterns}}, $s);
    },
    'linesafteruntilpattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{linesafteruntilpatterns}}, $s);
    },
    'linesbefore' => sub {
        my ($s, $settings) = @_;
        $settings->{linesbefore} = int($s);
    },
    'maxlinelength' => sub {
        my ($s, $settings) = @_;
        $settings->{maxlinelength} = int($s);
    },
    'out-archiveext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{out_archiveextensions});
    },
    'out-archivefilepattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{out_archivefilepatterns}}, $s);
    },
    'out-dirpattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{out_dirpatterns}}, $s);
    },
    'out-ext' => sub {
        my ($s, $settings) = @_;
        $settings->add_exts($s, $settings->{out_extensions});
    },
    'out-filepattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{out_filepatterns}}, $s);
    },
    'out-linesafterpattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{out_linesafterpatterns}}, $s);
    },
    'out-linesbeforepattern' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{out_linesbeforepatterns}}, $s);
    },
    'search' => sub {
        my ($s, $settings) = @_;
        push (@{$settings->{searchpatterns}}, $s);
    }
};

my $flag_action_hash = {
    'allmatches' => sub {
        my ($settings) = @_;
        $settings->{firstmatch} = 0;
    },
    'archivesonly' => sub {
        my ($settings) = @_;
        $settings->{archivesonly} = 1;
        $settings->{searcharchives} = 1;
    },
    'debug' => sub {
        my ($settings) = @_;
        $settings->{debug} = 1;
        $settings->{verbose} = 1;
    },
    'excludehidden' => sub {
        my ($settings) = @_;
        $settings->{excludehidden} = 1;
    },
    'firstmatch' => sub {
        my ($settings) = @_;
        $settings->{firstmatch} = 1;
    },
    'help' => sub {
        my ($settings) = @_;
        $settings->{printusage} = 1;
    },
    'includehidden' => sub {
        my ($settings) = @_;
        $settings->{excludehidden} = 0;
    },
    'listdirs' => sub {
        my ($settings) = @_;
        $settings->{listdirs} = 1;
    },
    'listfiles' => sub {
        my ($settings) = @_;
        $settings->{listfiles} = 1;
    },
    'listlines' => sub {
        my ($settings) = @_;
        $settings->{listlines} = 1;
    },
    'multilinesearch' => sub {
        my ($settings) = @_;
        $settings->{multilinesearch} = 1;
    },
    'noprintmatches' => sub {
        my ($settings) = @_;
        $settings->{printresults} = 0;
    },
    'norecursive' => sub {
        my ($settings) = @_;
        $settings->{recursive} = 0;
    },
    'nosearcharchives' => sub {
        my ($settings) = @_;
        $settings->{searcharchives} = 0;
    },
    'printmatches' => sub {
        my ($settings) = @_;
        $settings->{printresults} = 1;
    },
    'recursive' => sub {
        my ($settings) = @_;
        $settings->{recursive} = 1;
    },
    'searcharchives' => sub {
        my ($settings) = @_;
        $settings->{searcharchives} = 1;
    },
    'uniquelines' => sub {
        my ($settings) = @_;
        $settings->{uniquelines} = 1;
    },
    'verbose' => sub {
        my ($settings) = @_;
        $settings->{verbose} = 1;
    },
    'version' => sub {
        my ($settings) = @_;
        $settings->{printversion} = 1;
    }
};

sub set_options_from_xml {
    my $options_hash = {};
    my $options_xml_hash = XMLin($SEARCHOPTIONSPATH);
    foreach my $i (0..$#{$options_xml_hash->{searchoption}}) {
        my $short = $options_xml_hash->{searchoption}->[$i]->{short};
        my $long = $options_xml_hash->{searchoption}->[$i]->{long};
        my $desc = $options_xml_hash->{searchoption}->[$i]->{content};
        $desc = plsearch::common::trim($desc);
        my $func = sub {};
        if (exists $arg_action_hash->{$long}) {
            $func = $arg_action_hash->{$long};
        } elsif (exists $flag_action_hash->{$long}) {
            $func = $flag_action_hash->{$long};
        }
        my $opt = new plsearch::SearchOption($short, $long, $desc, $func);
        $options_hash->{$long} = $opt;
        if ($short) {
            $options_hash->{$short} = $options_hash->{$long};
        }
    }
    return $options_hash;
}

sub new {
    my $class = shift;
    my $self = {
        options => set_options_from_xml(),
    };
    bless $self, $class;
    return $self;
}

sub settings_from_args {
    my ($self, $args) = @_;
    my $settings = new plsearch::SearchSettings();
    my $i = 0;
    my @errs;
    while ($i <= $#{$args}) {
        my $arg = $args->[$i];
        if ($arg =~ /^\-+/) {
            $arg =~ s/^\-+//;
            if (exists $self->{options}->{$arg}) {
                my $opt = $self->{options}->{$arg};
                my $long = $opt->{longarg};
                if (exists $arg_action_hash->{$long}) {
                    if (exists $args->[$i+1]) {
                        my $val = $args->[$i+1];
                        &{$arg_action_hash->{$long}}($val, $settings);
                        $i++;
                    } else {
                        push(@errs, "Missing value for $arg");
                    }
                } elsif (exists $flag_action_hash->{$long}) {
                    &{$flag_action_hash->{$long}}($settings);
                }


            } else {
                push(@errs, "Invalid option: $arg");
            }
        } else {
            $settings->{startpath} = $arg;
        }
        $i++;
    }
    return ($settings, \@errs);
}

sub usage {
    my $self = shift;
    print $self->get_usage_string();
}

sub get_usage_string {
    my $self = shift;
    my $usage = "Usage:\n plsearch.pl [options] -s <searchpattern> <startpath>\n\nOptions:\n";
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
