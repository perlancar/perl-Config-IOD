package Config::IOD::Document;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;
use Carp;

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
    #croak "Please specify _parsed" unless $attrs{_parsed};
    bless \%attrs, $class;
}

# all _validate_*() methods return ($err_msg, $validated_val)

sub _validate_section {
    my ($self, $name) = @_;
    $name =~ s/\A\s+//;
    $name =~ s/\s+\z//;
    if (!length($name)) { return ("Section name must be non-zero string") }
    if ($name =~ /\R|\]/) { return ("Section name must not contain ] or newline") }
    return ("", $name);
}

sub _validate_key {
    my ($self, $name) = @_;
    $name =~ s/\A\s+//;
    $name =~ s/\s+\z//;
    if (!length($name)) { return ("Key name must be non-zero string") }
    if ($name =~ /\R|=/) { return ("Key name must not contain = or newline") }
    return ("", $name);
}

sub _validate_value {
    my ($self, $value) = @_;
    $value =~ s/\s+\z//;
    if ($value =~ /\R/) { return ("Value must not contain newline") }
    return ("", $value);
}

sub _validate_comment {
    my ($self, $comment) = @_;
    if ($comment =~ /\R/) { return ("Comment must not contain newline") }
    return ("", $comment);
}

sub _validate_linum {
    my ($self, $value) = @_;
    if ($value < 1) { return ("linum must be at least 1") }
    if ($value > @{$self->{_parsed}}) { return ("linum must not be larger than number of document's lines") }
    return ("", $value);
}

sub _blank_line {
    ["B", "\n"];
}

sub _find_section {
    my $self = shift;
    my $opts;
    if (ref($_[0]) eq 'HASH') {
        $opts = shift;
    } else {
        $opts = {};
    }
    my ($name) = @_;

    my @res;

    my $linum = 0;
    for my $line (@{ $self->{_parsed} }) {
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

sub _find_key {
    my $self = shift;
    my $opts;
    if (ref($_[0]) eq 'HASH') {
        $opts = shift;
    } else {
        $opts = {};
    }
    my ($section, $name) = @_;

    my @res;

    my $linum = 0;
    my $cur_section = $self->{_parser}{default_section};
    for my $line (@{ $self->{_parsed} }) {
        $linum++;
        if ($line->[COL_TYPE] eq 'S') {
            $cur_section = $line->[COL_S_SECTION];
            next;
        }
        next unless $line->[COL_TYPE] eq 'K';
        next unless $cur_section eq $section;
        next unless $line->[COL_K_KEY] eq $name;
        return $linum unless $opts->{all};
        push @res, $linum;
    }
    return undef unless $opts->{all};
    return @res;
}

sub insert_section {
    my $self = shift;
    my $opts;
    if (ref($_[0]) eq 'HASH') {
        $opts = shift;
    } else {
        $opts = {};
    }

    my ($err, $name) = $self->_validate_section($_[0]);
    croak $err if $err;

    my $p = $self->{_parsed};

    if (defined $opts->{comment}) {
        ($err, $opts->{comment}) = $self->_validate_comment($opts->{comment});
        croak $err if $err;
    }

    if ($self->_find_section($name)) {
        if ($opts->{ignore}) {
            return;
        } else {
            croak "Can't insert section '$name': already exists";
        }
    }

    my $linum;
    if (defined $opts->{linum}) {
        ($err, $opts->{linum}) = $self->_validate_linum($opts->{linum});
        croak $err if $err;
        $linum = $opts->{linum};
    } elsif ($opts->{top}) {
        $linum = $self->_find_section;
        $linum //= 1;
    } else {
        $linum = @$p + 1;
    }

    splice @$p, $linum-1, 0, [
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
    $linum;
}

sub insert_key {
    my $self = shift;
    my $opts;
    if (ref($_[0]) eq 'HASH') {
        $opts = shift;
    } else {
        $opts = {};
    }

    my ($err_section, $section) = $self->_validate_section($_[0]);
    croak $err_section if $err_section;
    my ($err_name, $name)       = $self->_validate_key($_[1]);
    croak $err_name if $err_name;
    my ($err_value, $value)     = $self->_validate_value($_[2]);
    croak $err_value if $err_value;

    my $p = $self->{_parsed};

    my $linum;
    my @lines_to_add;

    push @lines_to_add, [
        'K',
        '', # COL_K_WS1
        $name, # COL_K_KEY
        '', # COL_K_WS2
        '', # COL_K_WS3
        $value, # COL_K_VALUE_RAW
        "\n", # COL_K_NL
    ];

    # find section
    my $linum_section;
    if (!($linum_section = $self->_find_section($section))) {
        if ($opts->{create_section}) {
            $linum_section = $self->insert_section($section);
            $linum = $linum_section + 1;
        } else {
            croak "Can't insert key '$name': unknown section '$section'";
        }
    }

    unless (defined $linum) {
        $linum = $self->_find_key($section, $name);
        if ($linum) {
            croak "Can't insert key '$name': already exists";
        } else {
            $linum = @$p + 1;
        }
    }

    if ($opts->{top}) {
        $linum = $linum_section+1; # XXX should be before other key
    }

    #XXX implement option: add
    #XXX implement option: ignore
    #XXX implement option: replace

    splice @$p, $linum-1, 0, @lines_to_add;
    $linum;
}

sub as_string {
    my $self = shift;

    my $abo = $self->{_parser}{allow_bang_only};

    my @str;
    my $linum = 0;
    for my $line (@{$self->{_parsed}}) {
        $linum++;
        my $type = $line->[COL_TYPE];
        if ($type eq 'B') {
            push @str, $line->[COL_B_RAW];
        } elsif ($type eq 'D') {
            push @str, join(
                "",
                ($abo ? $line->[COL_D_COMMENT_CHAR] : ";"),
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

=head2 $doc->insert_section([\%opts, ]$name)

Insert empty section named C<$name>. Die on failure.

Options:

=over

=item * ignore => bool

If set to 1, then if section already exists will do nothing instead of die.

=item * top => bool

If set to 1, will insert before any other section. By default will insert at the
end of document. See also: C<linum>.

=item * comment => str

Optional. Comment to add at the end of section line.

=item * linum => posint

Optional. Insert at this specific line number. Ignores C<top>.

=> =back

=head2 $doc->insert_key([\%opts, ]$section, $key, $value)

Insert a key named C<$name> with value C<$value> under C<$section>. Die on
failure.

Options:

=over

=item * create_section => bool

If set to 1, will create section (at the end of document) if it doesn't exist.

=item * add => bool

If set to 1, will add another key if key with the same name already exists.
Conflicts with C<ignore> and <replace>.

=item * ignore => bool

If set to 1, will do nothing if key already exists. Conflicts with C<add> and
C<replace>.

=item * replace => bool

If set to 1, will replace (all) previous key if key already exists. Conflicts
with C<add> and C<ignore>.

=item * top => bool

=back


=head1 SEE ALSO

L<Config::IOD>
