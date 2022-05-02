package Config::IOD::INI;

use 5.010001;
use strict;
use warnings;

use parent qw(Config::IOD);

# AUTHORITY
# DATE
# DIST
# VERSION

sub new {
    my ($class, %attrs) = @_;
    $attrs{enable_directive} //= 0;
    $attrs{enable_encoding}  //= 0;
    $attrs{enable_quoting}   //= 0;
    $attrs{enable_bracket}   //= 0;
    $attrs{enable_brace}     //= 0;
    $attrs{enable_tilde}     //= 0;
    $class->SUPER::new(%attrs);
}

1;
#ABSTRACT: Read and write INI configuration files

=head1 SYNOPSIS

 use Config::IOD::INI;
 my $iod = Config::IOD->new();

Read INI document from a file or string, return L<Config::IOD::Document> object:

 my $doc = $iod->read_file("/path/to/some.ini");
 my $doc = $iod->read_string("...");

See Config::IOD::Document for methods available for C<$doc>.


=head1 DESCRIPTION

This module is just a L<Config::IOD> subclass. It uses the following defaults to
make the reader's behavior closer to a typical "regular INI files parser".

    enable_directive = 0
    enable_encoding  = 0
    enable_quoting   = 0
    enable_bracket   = 0
    enable_brace     = 0
    enable_tilde     = 0


=head1 SEE ALSO

L<Config::IOD>

=cut
