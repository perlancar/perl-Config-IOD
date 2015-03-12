package Config::IOD;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

use constant +{
    COL_TYPE => 0,

    COL_B_RAW => 1,

    COL_D_COMMENT_CHAR => 1,
    COL_D_WS1 => 2,
    COL_D_WS2 => 3,
    COL_D_DIRECTIVE => 4,
    COL_D_WS3 => 5,
    COL_D_ARGS_RAW => 6,
    COL_D_NL => 7,

    COL_C_WS1 => 1,
    COL_C_COMMENT_CHAR => 2,
    COL_C_COMMENT => 3,
    COL_C_NL => 4,

    COL_S_WS1 => 1,
    COL_S_WS2 => 2,
    COL_S_SECTION => 3,
    COL_S_WS3 => 4,
    COL_S_WS4 => 5,
    COL_S_COMMENT_CHAR => 6,
    COL_S_COMMENT => 7,
    COL_S_NL => 8,

    COL_K_WS1 => 1,
    COL_K_KEY => 2,
    COL_K_WS2 => 3,
    COL_K_WS3 => 4,
    COL_K_VALUE_RAW => 5,
    COL_K_NL => 6,
};

use parent qw(Config::IOD::Base);

sub _init_read {
    my $self = shift;

    $self->SUPER::_init_read;
    $self->{_res} = [];
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

    my $res = $self->{_res};

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
                $self->_err("Unknown directive '$directive'");
            }
            next LINE;
        }

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

        $self->_err("Invalid syntax");
    }

    $res;
}

sub _res_as_string {
    my ($self, $res) = @_;

    my @str;
    my $linum = 0;
    for my $line (@$res) {
        $linum++;
        my $type = $line->[COL_TYPE];
        if ($type eq 'B') {
            push @str, $line->[COL_B_RAW];
        } elsif ($type eq 'D') {
            push @str, join(
                "",
                ($self->{allow_bang_only} ? $line->[COL_D_COMMENT_CHAR] : ";"),
                $line->[COL_D_WS1], "!",
                $line->[COL_D_WS2],
                $line->[COL_D_DIRECTIVE],
                $line->[COL_D_WS3],
                $line->[COL_D_ARGS_RAW],
                $line->[COL_D_NL],
            );
        } elsif ($type eq 'C') {
            push @str, join(
                "",
                $line->[COL_C_WS1],
                $line->[COL_C_COMMENT_CHAR],
                $line->[COL_C_COMMENT],
                $line->[COL_C_NL],
            );
        } elsif ($type eq 'S') {
            push @str, join(
                "",
                $line->[COL_S_WS1], "[",
                $line->[COL_S_WS2],
                $line->[COL_S_SECTION],
                $line->[COL_S_WS3], "]",
                $line->[COL_S_WS4] // '',
                $line->[COL_S_COMMENT_CHAR] // '',
                $line->[COL_S_COMMENT] // '',
                $line->[COL_S_NL],
            );
        } elsif ($type eq 'K') {
            push @str, join(
                "",
                $line->[COL_K_WS1],
                $line->[COL_K_KEY],
                $line->[COL_K_WS2], "=",
                $line->[COL_K_WS3],
                $line->[COL_K_VALUE_RAW],
                $line->[COL_K_NL],
            );
        } else {
            die "BUG: Unknown type '$type' in line $linum";
        }
    }

    join "", @str;
}

# XXX handle decoding in get_value

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

 my $section = $ini->get_section("Section"); # a hashref of param=>values
 my $val = $ini->get_value("Section", "Parameter");

 # not yet implemented
 $ini->add_section("Section2"); # empty section
 $ini->add_section("Section3", {Param=>Value, ...}); # section with values
 $ini->delete_section("Section2");
 $ini->set_value("Section", "Param", "New Value");
 $ini->delete_value("Section", "Param");

 $ini->as_tree;

 # dump back as string
 $ini->as_string;


=head1 EXPRESSION

# INSERT_BLOCK: Config::IOD::Base expression


=head1 ATTRIBUTES

# INSERT_BLOCK: Config::IOD::Base attributes


=head1 METHODS


=head1 SEE ALSO

L<IOD> - specification

L<Config::IOD::Reader> - if you just need to read a configuration file, you
should probably use this module instead. It's lighter, faster, and has a simpler
interface.

L<IOD::Examples> - sample documents

=cut
