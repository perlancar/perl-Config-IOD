package Config::IOD;

# TODO: option to only allow include if owner is the same

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Cwd qw(abs_path);
use Exporter::Lite;
use File::Slurp;
use File::chdir;

our @EXPORT_OK = qw();

# VERSION

use constant {
    COL_RAW     => 0,
    COL_INCLUDE => 1, # 1 if line is from included file
    COL_TYPE    => 2, # [BCDPS]

    COL_D_NAME  => 3,
    COL_D_ARGS  => 4,
    COL_P_NAME  => 3,
    COL_P_VALUE => 4,
    COL_S_NAMES => 3,
};

our %SPEC;

our $re_hexdig  = qr/(?:[0-9A-Fa-f])/;
# XXX: wide hex not supported yet
our $re_strpart = qr/(?:
                         [^"\\]+ |
                         \\[tnrfbae"'\\\$] |
                         \\0[0-7]{0,3} |
                         \\x$re_hexdig {1,2}
                     )/x;
our $re_string  = qr/(?: $re_strpart* )/x;
# simpler, faster regex for matching qs on left s of =
our $re_quotedl = qr/(?:"(?:[^"\\]|\\"|\\)*")/x;
# for matching on right side
our $re_quotedr = qr/(?:".*?")/x;
our $re_quoted  = qr/(?:\"$re_string\")/x;
our $re_S       = qr/^\s*
                     \[ (?:(?<name_quoted>$re_quotedl) | (?<name_bare>[^"\]]))\]
                     \s*
                    /x;
our $re_P       = qr/^\s*
                     (?:(?<name_quoted>$re_quotedl) | (?<name_bare>[^"=]+?) )
                     \s* = \s*
                     (?:(?<value_quoted>$re_quotedr) \s* | (?<value_bare>.*?) )
                     $
                    /x;
our $re_D_arg   = qr/$re_quotedl | [^"\s]+/x;
our $re_D_args  = qr/(?:$re_D_arg (\s+ $re_D_arg)*)/x;
our $re_D       = qr/^\s*
                     [;#]
                     !(?<name>\w+) (?<args> \s+ $re_D_args )?
                     \s*$
                    /x;

sub _fmtmsg {
    my ($self, $msg, $prefix) = @_;
    join("",
         $prefix // "",
         (defined($self->{_curfile}) ?
              " in file `$self->{_curfile}`" : ""),
         (defined($self->{_curline}) ?
              " at line #$self->{_curline}" : ""),
        ": $msg"
    );
}

sub _dieline {
    my ($self, $msg) = @_;
    die $self->_fmtmsg($msg, "Parse error");
}

sub _warnline {
    my ($self, $msg) = @_;
    $log->warn($self->_fmtmsg($msg), "Parse warning");
}

sub _parse_quoted {
    my ($self, $str, $quoted) = @_;
    $quoted //= 1;
    state $escapes = {
        '"' => '"', "'" => "'", "\\" => "\\",
        "r" => "\r", "t" => "\t", "n" => "\n",
        "f" => "\f", "a" => "\a", "b" => "\b",
        "e" => "\e", '$' => '$',
    };
    if ($quoted) {
        $str =~ s/\A"//
            or $self->_dieline("String without opening quotes: $str");
        $str =~ s/"\z//
            or $self->_dieline("String without closing quotes: $str");
    }
    if ($str !~ /\A$re_string\z/) {
        $self->_dieline("Invalid string syntax: $str");
    }
    my @el = $str =~ /($re_strpart)/g;
    for (@el) {
        if (/^\\0(.*)/) {
            $_ = chr(oct($1));
        } elsif (/^\\x(.+)/) {
            $_ = chr(hex($1));
        } elsif (/^\\(.)/) {
            $_ = $escapes->{$1};
        }
    }
    join "", @el;
}

# input must be valid raw args
sub _split_args {
    my ($self, $args) = @_;
    my @args;
    while ($args =~ s/($re_D_arg)\s*//) {
        push @args, $1;
    }
    @args;
}

sub _include {
    my ($self, $filename) = @_;
    if (@{$self->{_include_stack}}) {
        $log->tracef("Including %s ...", $filename);
    } else {
        $log->tracef("Loading INI configuration %s ...", $filename);
    }
    my $absfilename = abs_path($filename);
    (-f $filename) && $absfilename or
        $self->_dieline("Can't load file $filename: not found (cwd=".
                    Cwd::cwd().")");
    $self->{_include_stack} //= [];
    if ($absfilename ~~ @{$self->{_include_stack}}) {
        $self->_dieline("Recursive include: $absfilename");
    }
    my $dir;
    if ($filename =~ m!/!) {
        $dir = $filename;
        $dir =~ s!(.+)/(.*)!$1!;
    } else {
        $dir = "";
    }

    my $ct;
    eval { $ct = read_file($filename) };
    my $eval_err = $@;
    if ($eval_err) {
        $self->_dieline("Can't load file $filename: $eval_err");
    }
    {
        my $tmp = sub {
            local $self->{_curline};
            local $self->{_curfile} = $absfilename;
            push @{$self->{_include_stack}}, $absfilename;
            $self->{_include_level}++;
            $self->_parse_raw($ct);
            $self->{_include_level}--;
            pop @{$self->{_include_stack}};
        };
        $CWD = $dir if length($dir);
        $tmp->();
    }
}

sub dirmeta_include { {phase=>1} }
sub dir_include {
    my ($self, $args) = @_;
    my $filename = $args->[0];
    $self->_include($filename);
}

sub dirmeta_defaults { {phase=>2} }
sub dir_defaults {
    my ($self, $args) = @_;
}

#sub dirmeta_merge { {phase=>??} }
#sub dir_merge {
#    my ($self, $args) = @_;
#}

# parse raw ini untuk an array of lines. each line is an arrayref [RAW_LINE,
# TYPE(*)?, (type-specific data ...)]. TYPE is either "B" (blank line), "C"
# (comment), "D" (directive), "S" (section), or "P" (parameter). * suffix
# indicates that line comes from an included file.
#
sub _parse_raw {
    my ($self, $raw) = @_;

    if (!ref($raw) || ref($raw) ne 'ARRAY') {
        $raw = [split /^/, $raw];
    }

    $self->{_curline} = 0;

    if ($self->{_include_level} == 0) {
        $self->{_lines} = [];
    }

    my @section_hooks;
    my @param_hooks;
    for my $line0 (@$raw) {
        $self->{_curline}++;
        my @line;
        $line[COL_RAW] = $line0;
        $line[COL_INCLUDE] = $self->{_include_level} > 0;

        if ($line0 !~ /\S/) {

            $line[COL_TYPE] = "B";

        } elsif ($line0 =~ /^\s*[;#](.*)/) {

            my $arg = $1;
            if ($arg =~ /^!\w+(?:\s+|$)/) {
                if ($line0 =~ $re_D) {
                    my $name = $+{name};
                    my $args0 = $+{args} // "";
                    my $meth = "dir_$name";
                    unless ($self->can($meth)) {
                        $self->_dieline("Unknown directive: $name");
                    }
                    my @args = $self->_split_args($args0);
                    $line[COL_TYPE]   = "D";
                    $line[COL_D_NAME] = $name;
                    $line[COL_D_ARGS] = \@args;
                    my $methmt = "dirmeta_$name";
                    unless ($self->can($methmt)) {
                        $self->_dieline("Directive doesn't have meta: $name");
                    }
                    my $meta = $self->$methmt($name);
                    if ($meta->{phase} == 1) {
                        $self->$meth(\@args);
                    }
                } else {
                    $self->_warnline("Invalid directive syntax");
                }
            } else {
                $line[COL_TYPE] = "C";
            }

        } elsif ($line0 =~ /^\s*\[(.*)\]/) {

            $line[COL_TYPE]    = "S";
            $line[COL_S_NAMES] = [$1]; # XXX

        } elsif ($line0 =~ /=/) {

            if ($line0 =~ $re_P) {
                my $name = defined($+{name_quoted}) ?
                    $self->_parse_quoted($+{name_quoted}) : $+{name_bare};
                my $value = defined($+{value_quoted}) ?
                    $self->_parse_quoted($+{value_quoted}) : $+{value_bare};
                $line[COL_TYPE]    = "P";
                $line[COL_P_NAME]  = $name;
                $line[COL_P_VALUE] = $value;
            } else {
                $self->_dieline("Invalid parameter assignment syntax");
            }

        } else {

            $self->_dieline("Unknown line: $line0");
        }

        push @{$self->{_lines}}, \@line;
    }

    # clean parsing work variables
    if ($self->{_include_level} == 0) {
        undef $self->{_curline};
    }
}

# parse array of lines into tree of section/params.
sub _parse_lines {
    my ($self) = @_;

    $self->{_next_section_hooks} = [];
    $self->{_next_param_hooks} = [];
    $self->{_cursection} = $self->{default_section};
    $self->{_tree} = {};

    for my $line (@{$self->{_lines}}) {
        my $type = $line->[COL_TYPE];
        if ($type eq 'S') {
            my $name = $line->[COL_S_NAMES];
            $self->{_cursection} = $name->[0]; # XXX
        } elsif ($type eq 'D') {
            my ($name, $args) = ($line->[COL_D_NAME], $line->[COL_D_ARGS]);
            my $methmt = "dirmeta_$name";
            my $meta = $self->$methmt($name);
            if ($meta->{phase} == 2) {
                my $meth = "dir_$name";
                $self->$meth($args);
            }
        } elsif ($type eq 'P') {
            my ($name, $value) = ($line->[COL_P_NAME], $line->[COL_P_VALUE]);
            $self->{_tree}{ $self->{_cursection} }{$name} = $value;
        }
    }

    # clean parseing work variables
    undef $self->{_cursection};
    undef $self->{_next_param_hooks};
    undef $self->{_next_section_hooks};

    $self->{_tree};
}

sub new {
    my ($class, %args) = @_;

    my $self = bless {}, $class;

    for my $k (keys %args) {
        die "Unknown argument: $k" unless
            $k =~ /\A(file|str|default_section)\z/;
        $self->{$k} = $args{$k};
    }
    $self->{default_section} //= 'DEFAULT';
    $self->{_include_stack} = [];

    if (defined($self->{file}) || defined($self->{str})) {
        if (defined $self->{file}) {
            $self->{_include_level} = -1;
            $self->_include($self->{file});
        } elsif (defined $self->{str}) {
            $self->{_include_level} = 0;
            $self->_parse_raw($self->{str});
        }
        $self->_parse_lines;
    }
    $self;
}

sub get_section {
    my ($self, $section) = @_;
    $self->{_tree}{$section};
}

sub get_value {
    my ($self, $section, $param) = @_;
    my $s = $self->get_section($section);
    return undef unless $s;
    $s->{$param};
}

sub as_tree {
    my ($self) = @_;
    $self->{_tree};
}

# procedural functions

#$SPEC{a} = {
#    summary => '',
#    description => '',
#    args => {
#    },
#};

1;
#ABSTRACT: Read and write IOD (INI On Drugs) files

=head1 SYNOPSIS

 # oo interface
 use Config::IOD;
 my $ini = Config::IOD->new(file => "file.ini");
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

 # procedural interface, Config::IOD is a shorter alias
 use Config::IOD qw(
     ini_get
     ini_get_section
     ini get_value
     ini_add_section ini_delete_section
     ini_add_value ini_set_value ini_delete_value
     ini_comment_value ini_uncomment_value
     ini_comment_section ini_uncomment_section
 );
 my $ini = ini_get("file.ini");
 my $section = ini_get_section("file.ini", "Section");
 my $value = ini_get_value("file.ini", "Section", "Parameter");
 ini_add_value("file.ini", , "Section", "Parameter", "Value");
 ...


=head1 ATTRIBUTES


=head1 METHODS


=head1 FAQ


=head1 SEE ALSO

File format specification: L<IOD>.

Other INI modules: L<Config::IniFiles>, L<Config::INI>, L<Config::INIPlus>, etc.

Other alternative formats: L<YAML>, L<JSON>, L<Config::General>, XML, etc.

The original blog post/discussion which leads to this module:
http://blogs.perl.org/users/steven_haryanto/2011/09/yaml-vs-ini-again-and-the-plan-for-yet-another-ini-module.html

This module uses L<Log::Any> logging framework.

This module has L<Rinci> metadata.

=cut
