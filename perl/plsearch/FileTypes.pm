###############################################################################
#
# FileTypes.pm
#
# Helper class for file types
#
###############################################################################

package plsearch::FileTypes;

use strict;
use warnings;

use XML::Simple;
use Data::Dumper;
use plsearch::common;
use plsearch::config;
use plsearch::FileType;
use plsearch::FileUtil;

sub get_file_type_hash {
    my $file_type_hash = {};
    my $file_type_xml_hash = XMLin($FILETYPESPATH);
    $file_type_xml_hash = $file_type_xml_hash->{filetype};
    my @types = keys %{$file_type_xml_hash};
    foreach my $t (@types) {
        my @exts = split(/\s+/, $file_type_xml_hash->{$t}->{extensions});
        $file_type_hash->{$t} = \@exts;
    }
    my @text = (@{$file_type_hash->{text}}, @{$file_type_hash->{code}},
        @{$file_type_hash->{xml}});
    $file_type_hash->{text} = \@text;
    my @searchable = (@{$file_type_hash->{text}}, @{$file_type_hash->{archive}},
        @{$file_type_hash->{binary}});
    $file_type_hash->{searchable} = \@searchable;
    return $file_type_hash;
}

sub new {
    my $class = shift;
    my $self = {
        file_types => get_file_type_hash(),
    };
    bless $self, $class;
    return $self;
}

sub from_name {
    my ($name) = @_;
    if (uc($name) eq 'TEXT') {
        return plsearch::FileType->TEXT;
    }
    if (uc($name) eq 'BINARY') {
        return plsearch::FileType->BINARY;
    }
    if (uc($name) eq 'ARCHIVE') {
        return plsearch::FileType->ARCHIVE;
    }
    return plsearch::FileType->UNKNOWN;
}

sub get_filetype {
    my ($self, $file) = @_;
    if ($self->is_text($file)) {
        return plsearch::FileType->TEXT;
    }
    if ($self->is_binary($file)) {
        return plsearch::FileType->BINARY;
    }
    if ($self->is_archive($file)) {
        return plsearch::FileType->ARCHIVE;
    }
    return plsearch::FileType->UNKNOWN;
}

sub is_archive {
    my ($self, $file) = @_;
    my $ext = plsearch::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{archive}}) {
        return 1;
    }
    return 0;
}

sub is_binary {
    my ($self, $file) = @_;
    my $ext = plsearch::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{binary}}) {
        return 1;
    }
    return 0;
}

sub is_text {
    my ($self, $file) = @_;
    my $ext = plsearch::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{text}}) {
        return 1;
    }
    return 0;
}

sub is_searchable {
    my ($self, $file) = @_;
    my $ext = plsearch::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{searchable}}) {
        return 1;
    }
    return 0;
}

sub is_unknown {
    my ($self, $file) = @_;
    my $ext = plsearch::FileUtil::get_extension($file);
    if (grep {$_ eq $ext} @{$self->{file_types}->{searchable}}) {
        return 0;
    }
    return 1;
}

1;

__END__
