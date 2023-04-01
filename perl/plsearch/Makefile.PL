use 5.005;
use strict;
use warnings FATAL => 'all';
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'plsearch',
    VERSION_FROM      => 'lib/plsearch.pm', # finds $VERSION, requires EU::MM from perl >= 5.5
    PREREQ_PM         => {'Test'        =>  0, }, # e.g., Module::Name => 1.1
    ABSTRACT_FROM     => 'lib/plsearch.pm', # retrieve abstract from module
    AUTHOR            => 'Cary Clark <clarkcb@gmail.com>',
    #LICENSE           => 'perl',
    #Value must be from legacy list of licenses here
    #https://metacpan.org/pod/Module::Build::API
);
