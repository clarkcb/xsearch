###############################################################################
#
# SearchSettings.pm
#
# Encapsulates the search settings
#
###############################################################################

package plsearch::SearchSettings;

use strict;
use warnings;

sub new {
    my $class = shift;
    my $self = {
        archivesonly => 0,
        debug => 0,
        dotiming => 0,
        excludehidden => 1,
        firstmatch => 0,
        in_archiveextensions => [],
        in_archivefilepatterns => [],
        in_dirpatterns => [],
        in_extensions => [],
        in_filepatterns => [],
        in_linesafterpatterns => [],
        in_linesbeforepatterns => [],
        linesafter => 0,
        linesaftertopatterns => [],
        linesafteruntilpatterns => [],
        linesbefore => 0,
        listdirs => 0,
        listfiles => 0,
        listlines => 0,
        maxlinelength => 150,
        multilinesearch => 0,
        out_archiveextensions => [],
        out_archivefilepatterns => [],
        out_dirpatterns => [],
        out_extensions => [],
        out_filepatterns => [],
        out_linesafterpatterns => [],
        out_linesbeforepatterns => [],
        printresults => 1,
        printusage => 0,
        printversion => 0,
        recursive => 1,
        searcharchives => 0,
        searchpatterns => [],
        startpath => '',
        uniquelines => 0,
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

sub add_exts {
    my ($self, $exts, $extaref) = @_;
    my @xs = split(',', $exts);
    foreach my $x (@xs) {
        push (@{$extaref}, $x);
    }
}

sub to_string {
    my $self = shift @_;
    my $s = "SearchSettings(";
    $s .= 'archivesonly=' . $self->bool_to_string($self->{archivesonly});
    $s .= ', debug=' . $self->bool_to_string($self->{debug});
    $s .= ', dotiming=' . $self->bool_to_string($self->{dotiming});
    $s .= ', excludehidden=' . $self->bool_to_string($self->{excludehidden});
    $s .= ', firstmatch=' . $self->bool_to_string($self->{firstmatch});
    $s .= ', in_archiveextensions=' . $self->aref_to_string($self->{in_archiveextensions});
    $s .= ', in_archivefilepatterns=' . $self->aref_to_string($self->{in_archivefilepatterns});
    $s .= ', in_dirpatterns=' . $self->aref_to_string($self->{in_dirpatterns});
    $s .= ', in_extensions=' . $self->aref_to_string($self->{in_extensions});
    $s .= ', in_filepatterns=' . $self->aref_to_string($self->{in_filepatterns});
    $s .= ', in_linesafterpatterns=' . $self->aref_to_string($self->{in_linesafterpatterns});
    $s .= ', in_linesbeforepatterns=' . $self->aref_to_string($self->{in_linesbeforepatterns});
    $s .= ', linesafter=' . $self->{linesafter};
    $s .= ', linesaftertopatterns=' . $self->aref_to_string($self->{linesaftertopatterns});
    $s .= ', linesafteruntilpatterns=' . $self->aref_to_string($self->{linesafteruntilpatterns});
    $s .= ', linesbefore=' . $self->{linesbefore};
    $s .= ', listdirs=' . $self->bool_to_string($self->{listdirs});
    $s .= ', listfiles=' . $self->bool_to_string($self->{listfiles});
    $s .= ', listlines=' . $self->bool_to_string($self->{listlines});
    $s .= ', maxlinelength=' . $self->{maxlinelength};
    $s .= ', multilinesearch=' . $self->bool_to_string($self->{multilinesearch});
    $s .= ', out_archiveextensions=' . $self->aref_to_string($self->{out_archiveextensions});
    $s .= ', out_archivefilepatterns=' . $self->aref_to_string($self->{out_archivefilepatterns});
    $s .= ', out_dirpatterns=' . $self->aref_to_string($self->{out_dirpatterns});
    $s .= ', out_extensions=' . $self->aref_to_string($self->{out_extensions});
    $s .= ', out_filepatterns=' . $self->aref_to_string($self->{out_filepatterns});
    $s .= ', out_linesafterpatterns=' . $self->aref_to_string($self->{out_linesafterpatterns});
    $s .= ', out_linesbeforepatterns=' . $self->aref_to_string($self->{out_linesbeforepatterns});
    $s .= ', printresults=' . $self->bool_to_string($self->{printresults});
    $s .= ', printusage=' . $self->bool_to_string($self->{printusage});
    $s .= ', printversion=' . $self->bool_to_string($self->{printversion});
    $s .= ', recursive=' . $self->bool_to_string($self->{recursive});
    $s .= ', searcharchives=' . $self->bool_to_string($self->{searcharchives});
    $s .= ', searchpatterns=' . $self->aref_to_string($self->{searchpatterns});
    $s .= ', startpath="' . $self->{startpath} . '"';
    $s .= ', uniquelines=' . $self->bool_to_string($self->{uniquelines});
    $s .= ', verbose=' . $self->bool_to_string($self->{verbose});
    $s .= ')';
    return $s;
}

1;

__END__
