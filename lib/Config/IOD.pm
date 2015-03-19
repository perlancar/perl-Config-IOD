package Config::IOD;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

use parent qw(Config::IOD::Base);

sub _init_read {
    my $self = shift;

    $self->SUPER::_init_read;
}

our $re_directive_abo =
    qr/^(;?)(\s*)!
       (\s*)(\w+)(\s*)(.*)
       (\R?)\z/x;
our $re_directive =
    qr/^(;)(\s*)!
       (\s*)(\w+)(\s*)(.*)
       (\R?)\z/x;

sub _read_string {
    my ($self, $str) = @_;

    my $res = [];

    my $directive_re = $self->{allow_bang_only} ?
        $re_directive_abo : $re_directive;

    my @lines = split /^/, $str;
    local $self->{_linum} = 0;
  LINE:
    for my $line (@lines) {
        $self->{_linum}++;

        # blank line
        if ($line !~ /\S/) {
            push @$res, [
                'B',
                $line, # RAW
            ];
            next LINE;
        }

        # key line
        if ($line =~ /^(\s*)([^=]+?)(\s*)=
                      (\s*)(.*?)
                      (\R?)\z/x) {
            push @$res, [
                'K',
                $1, # COL_K_WS1
                $2, # COL_K_KEY
                $3, # COL_K_WS2
                $4, # COL_K_WS3
                $5, # COL_K_VALUE_RAW
                $6, # COL_K_NL
            ];
            next LINE;
        }

        # section line
        if ($line =~ /^(\s*)\[(\s*)(.+?)(\s*)\]
                      (?: (\s*)([;#])(.*))?
                      (\R?)\z/x) {
            push @$res, [
                'S',
                $1, # COL_S_WS1
                $2, # COL_S_WS2
                $3, # COL_S_SECTION
                $4, # COL_S_WS3
                $5, # COL_S_WS4
                $6, # COL_S_COMMENT_CHAR
                $7, # COL_S_COMMENT
                $8, # COL_S_NL
            ];
            next LINE;
        }

        # directive line
        if ($line =~ s/$directive_re//) {
            push @$res, [
                'D',
                $1, # COL_D_COMMENT_CHAR
                $2, # COL_D_WS1
                $3, # COL_D_WS2
                $4, # COL_D_DIRECTIVE
                $5, # COL_D_WS3
                $6, # COL_D_ARGS_RAW
                $7, # COL_D_NL
            ];
            my $directive = $4;
            if ($self->{allow_directives}) {
                $self->_err("Directive '$directive' is not in ".
                                "allow_directives list")
                    unless grep { $_ eq $directive }
                        @{$self->{allow_directives}};
            }
            if ($self->{disallow_directives}) {
                $self->_err("Directive '$directive' is in ".
                                "disallow_directives list")
                    if grep { $_ eq $directive }
                        @{$self->{disallow_directives}};
            }
            my $args = $self->_parse_command_line($6);
            if (!defined($args)) {
                $self->_err("Invalid arguments syntax '$line'");
            }
            if ($directive eq 'include') {
                my $path;
                if (! @$args) {
                    $self->_err("Missing filename to include");
                } elsif (@$args > 1) {
                    $self->_err("Extraneous arguments");
                } else {
                    $path = $args->[0];
                }
                my $res = $self->_push_include_stack($path);
                if ($res->[0] != 200) {
                    $self->_err("Can't include '$path': $res->[1]");
                }
                $path = $res->[2];
                $self->_read_string($self->_read_file($path));
                $self->_pop_include_stack;
            } elsif ($directive eq 'merge') {
            } elsif ($directive eq 'noop') {
            } else {
                if ($self->{ignore_unknown_directive}) {
                    # assume a regular comment
                    goto L1;
                } else {
                    $self->_err("Unknown directive '$directive'");
                }
            }
            next LINE;
        }

      L1:
        # comment line
        if ($line =~ /^(\s*)([;#])(.*?)
                      (\R?)\z/x) {
            push @$res, [
                'C',
                $1, # COL_C_WS1
                $2, # COL_C_COMMENT_CHAR
                $3, # COL_C_COMMENT
                $4, # COL_C_NL
            ];
            next LINE;
        }

        $self->_err("Invalid syntax");
    }

    # make sure we always end with newline
    if (@$res) {
        $res->[-1][-1] .= "\n"
            unless $res->[-1][-1] =~ /\R\z/;
    }

    require Config::IOD::Document;
    Config::IOD::Document->new(_parser=>$self, _parsed=>$res);
}

1;
#ABSTRACT: Read and write IOD configuration files

=head1 SYNOPSIS

 use Config::IOD;
 my $iod = Config::IOD->new(
     # list of known attributes, with their default values
     # default_section     => 'GLOBAL',
     # enable_encoding     => 1,
     # enable_quoting      => 1,
     # enable_backet       => 1,
     # enable_brace        => 1,
     # allow_encodings     => undef, # or ['base64','json',...]
     # disallow_encodings  => undef, # or ['base64','json',...]
     # allow_directives    => undef, # or ['include','merge',...]
     # disallow_directives => undef, # or ['include','merge',...]
     # allow_bang_only     => 1,
     # enable_expr         => 0,
 );

Read IOD document from a file or string, return L<Config::IOD::Document> object:

 my $doc = $iod->read_file("/path/to/some.iod");
 my $doc = $iod->read_string("...");

See Config::IOD::Document for methods available for C<$doc>.


=head1 DESCRIPTION

This module is a round-trip parser for L<IOD> configuration format. Round-trip
means all whitespaces and comments are preserved, so you get byte-by-byte
equivalence if you dump back the parsed document into string.

Aside from parsing, methods for modifying IOD documents (add/delete sections &
keys, etc) are also provided.

If you only need to read IOD configuration files, you might want to use
L<Config::IOD::Reader> instead.


=head1 ATTRIBUTES

# INSERT_BLOCK: Config::IOD::Base attributes


=head1 METHODS

=head2 new(%attrs) => obj

=head2 $reader->read_file($filename) => obj

Read IOD configuration from a file. Return L<Config::IOD::Document> instance.
Die on errors.

=head2 $reader->read_string($str) => obj

Read IOD configuration from a string. Return L<Config::IOD::Document> instance.
Die on errors.


=head1 SEE ALSO

L<IOD> - specification

L<Config::IOD::Reader> - if you just need to read a configuration file, you
should probably use this module instead. It's lighter, faster, and has a simpler
interface.

L<IOD::Examples> - sample documents

=cut
