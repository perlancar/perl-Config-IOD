package Config::IOD::Document;

# DATE
# VERSION

use 5.010;
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

sub new {
    my ($class, %attrs) = @_;
    die "Please specify _raw" unless $attrs{_raw};
    bless \%attrs, $class;
}

sub _find_section {
    my ($self, $name, $opts) = @_;
    $opts //= {};

    my @res;

    my $linum = 0;
    for my $line (@{ $self->{_raw} }) {
        $linum++;
        next unless $line->[COL_TYPE] eq 'S';
        if (defined $name) {
            next unless $line->[COL_S_SECTION] eq $name;
        }
        return $linum unless $opts->{all};
        push @res, $linum;
    }
    return undef unless $opts->{all};
    return @res;
}

sub insert_section {
    my ($self, $name, $opts) = @_;
    $opts //= {};

    my $raw = $self->{_raw};

    $name =~ s/\A\s+//s;
    $name =~ s/\s+\z//s;

    if (defined $opts->{comment}) {
        $opts->{comment} =~ s/\R//g;
    }

    if ($self->_find_section($name)) {
        if ($opts->{ignore}) {
            return;
        } else {
            die "Can't insert section '$name': already exists";
        }
    }

    my $linum;
    my @lines_to_add;

    push @lines_to_add, [
        'S',
        '', # COL_S_WS1
        '', # COL_S_WS2
        $name, # COL_S_SECTION
        '', # COL_S_WS3
        defined($opts->{comment}) ? ' ' : undef, # COL_S_WS4
        defined($opts->{comment}) ? ';' : undef, # COL_S_COMMENT_CHAR
        $opts->{comment}, # COL_S_COMMENT
        "\n", # COL_S_NL
    ];

    if ($opts->{top}) {
        $linum = $self->_find_section;
        if ($linum) {
            push @lines_to_add, ['B', "\n"];
        }
        $linum //= 1;
    } else {
        $linum = @$raw + 1;
        if (!$raw->[$linum] ||
                $raw->[$linum] && $raw->[$linum][COL_TYPE] ne 'B') {
            unshift @lines_to_add, ['B', "\n"];
        }
    }

    splice @$raw, $linum-1, 0, @lines_to_add;
}

sub as_string {
    my $self = shift;

    my @str;
    my $linum = 0;
    for my $line (@{$self->{_raw}}) {
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

use overload '""' => \&as_string;

1;
# ABSTRACT: Represent IOD document

=head1 SYNOPSIS

See L<Config::IOD>


=head1 ATTRIBUTES


=head1 METHODS

=head2 new(%attrs) => obj

=head2 $doc->insert_section($name[, \%opts])

Insert empty section named C<$name>. Die on failure.

Options:

=over

=item * ignore => bool

If set to 1, then if section already exists will simply return instead of die.

=item * top => bool

If set to 1, will insert before any other section. By default will insert at the
end of document.

=item * comment => str

Optional. Comment to add at the end of section line.

=back


=head1 SEE ALSO

L<Config::IOD>
