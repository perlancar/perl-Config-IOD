package Config::Ini::OnDrugs;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Exporter::Lite;
our @EXPORT_OK = qw();

# VERSION

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
our $re_quotedl = qr/(?:(?:"[^"\\]|\\"|\\)*")/x;
# for matching on right side
our $re_quotedr = qr/(?:".*")/x;
our $re_quoted  = qr/(?:\"$re_string\")/x;
our $re_S       = qr/^\s*
                     \[ (?:(?<name_quoted>$re_quotedl) | (?<name_bare>[^"\]]))\]
                     \s*
                    /x;
our $re_P       = qr/^\s*
                     (?:(?<name_quoted>$re_quotedl) | (?<name_bare>[^"=]+) )
                     \s* = \s*
                     (?:(?<value_quoted>$re_quotedr) \s* | (?<value_bare>.*?) )
                     $
                    /x;
our $re_D_arg   = qr/$re_quotedl | [^"\s]+/x;
our $re_D_args  = qr/(?:$re_D_arg (\s+ $re_D_arg)*)/x;
our $re_D       = qr/^\s*
                     [;#]
                     !(?<name>\w+) (?: \s+ $re_D_args )?
                     \s*$
                    /x;

sub _dieline {
    my ($self, $msg) = @_;
    die "Parse error".(defined($self->{_curline}) ?
                           " line #$self->{_curline}" : "").
        ": $msg";
}

sub _warnline {
    my ($self, $msg) = @_;
    $log->warn("Parse error".(defined($self->{_curline}) ?
                                  " line #$self->{_curline}" : "").
                                      ": $msg");
}

sub _parse_quoted {
    my ($self, $str, $quoted) = @_;
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
sub __split_args {
    my ($args) = @_;
    my @args;
    while ($args =~ s/($re_D_arg)\s*//) {
        push @args, $1;
    }
    @args;
}

# parsing: create @lines where each line is [RAW_LINE, TYPE, (type-specific data
# ...)]. TYPE is either "B" (blank line), "C" (comment), "D" (directive), "S"
# (section), or "P" (parameter). "B" and "C" lines do not have type-specific
# data.
#
# data for "P":

sub _parse {
    my ($self, $raw) = @_;

    if (!ref($raw) || ref($raw) ne 'ARRAY') {
        $raw = [split /^/, $raw];
    }
    my @lines;
    $self->{_curline} = 0;
    for my $line0 (@$raw) {
        $self->{_curline}++;
        my @line = ($line0);
        if ($line0 !~ /\S/) {
            push @line, "B";
        } elsif ($line0 =~ /^\s*[;#](.*)/) {
            my $arg = $1;
            if ($arg =~ /^\!\w+(?:\s+|$)/) {
                if ($arg =~ $re_D) {
                    push @line, "D", $+{name}, __split_args($+{args});
                } else {
                    $self->_warnline("Invalid directive syntax");
                }
            } else {
                push @line, "C";
            }
        } elsif ($line0 =~ /^\s*\[/) {
            push @line, "S", ;
        } elsif ($line0 =~ /=/) {
            push @line, "P", ;
        } else {
            $self->_dieline("Unknown line: $line0");
        }

        push @lines, \@line;
    }
    $self->{lines} = \@lines;
}

sub new {
    my ($class, $raw) = @_;
    my $self = bless {}, $class;
    $self->_parse($raw) if defined($raw);
    $self;
}

$SPEC{a} = {
    summary => '',
    description => '',
    args => {
    },
};

1;
#ABSTRACT: Yet another INI reader/writer (round trip, includes, variables, nest)
__END__

=head1 SYNOPSIS

 # oo interface
 use Config::Ini::OnDrugs;
 my $ini = Config::Ini::

 # procedural interface
 use Config::Ini::OnDrugs qw(
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


=head1 DESCRIPTION

This module provides INI reading/writing class/functions. There are several
other INI modules on CPAN; this one focuses on round trip parsing, includes,
variables. The goal is to provide a usable format for configuration files
suitable for automation (programatic modification) as well as editing by humans.

=head2 What is round trip parsing, and why it is useful?

It means preserving everything in the file, including comments and formatting
(indents, whitespaces). In other words, if you load the INI file and dump it
again, the resulting dump will be identical to the original file. If you modify
just one parameter, the rest will be identical to the original (including
whitespaces).

Being round trip safe is useful for humans, because some of the things that are
useful to humans are in the comments and whitespaces, which are not significant
to machines.

Example:

 ; Important, only set between 2 and 40, otherwise it will explode!!!
 ; In fact, only set between 2 and 27.5, other values are bunk!!
 frob        =  2.3
 plunk       = 20.1
 thingamagic = 27.4

is much more useful than:

 frob=2.3
 plunk=20.1
 thingamagic=2.5

Most other formats do not provide round trip parser, e.g. L<JSON>, L<YAML>,
L<Config::General> (Apache-style), XML; they all lose comments. They are good
for automation but not ideal for humans. (Note: JSON documentation mentions the
phrase "round trip", but it uses the phrase to mean integrity of values, not
preserving comments/whitespaces.)


=head1 INI::OD FORMAT SPECIFICATION

Since the INI format does not have any formal specification, here is the
specification for INI as used by this module (from here on: Ini::OD). Ini::OD is
specified to be compatible with most of the INI files out there, while also
introduce several useful extensions.

An INI file is a text file containing either comment lines, directive lines,
blank lines, section lines, and parameter lines.

=head2 Comment line

A comment line begins with ; or # as it's first nonblank character.

=head2 Directive line

A directive line is a special comment line, starting with an exclamation mark
("!") followed by a directive name and zero or more arguments. An invalid
directive will be ignored and assumed to be a normal command (with warnings).

 #!directivename
 ;!directivename arg ...

Directives influence parsing and turn on/off features. Known directives will be
explained later in the text.

=head2 Section line

A section line introduces a section:

 [Section Name]
 ["quoted [] section name"]
 []
 [""]

Whitespace before the "[" token is allowed. To write a section name with
problematic characters (like "\n", "\0", "]", etc.), use quotes.

Ini::OD allows nested section using this syntax:

 [Outer][Inner][Even more inner]...

=head2 Parameter line

Parameter lines specify name value pairs:

 Parameter name = value
   Parameter2=value ; this is not a comment and part of value

Parameter name and value can be quoted:

 "Contains\nnewline" = "\0"

Whitespace before parameter name, and whitespaces between the "=" character are
allowed and ignored. Trailing whitespace is not ignored for unquoted values.

To specify a C<null> (C<undef>) value, use the !null directive and an empty
value:

 ; empty string
 param1=

 ;!null
 param2=

To specify an array value, use multiple lines:

 ; ["foo", "bar"]
 param=foo
 param=bar

To specify an array with a single value, use the !array directive:

 ; an array [foo]
 ;!array
 param=foo

 ; a string "foo"
 param2=foo

To specify an array with an empty element:

 ; []
 ;!array 0
 param=

To specify hash value, use nested section:

 [Section]
 ; param is {foo=>"1 ; a", bar=>"2"}
 [Section][param]
 foo=1 ; a
 bar="2"

Normally a parameter line should occur after section line, so that parameter
belongs to the section. But a parameter line is also allowed before section
line, in which it will belong to the default section specified in the parser.

=head2 Quoting

Quoting is done with the double-quote (L<">) character. Known escapes are \',
\", \\, \r (linefeed), \f (formfeed), \$ (literal $), \n (newline), \t (tab), \b
(backspace), \a (bell), \0, octal form ("\0377"), hex form ("\xff") and wide-hex
form (\x{263a}).

Quoting is allowed for section name in section line,

=head2 Includes

You can include another file using the !include directive:

 ;!include "/My Home/foo.ini"

=head2 Variables and calculations

You can use variables and calculations using the !expr directive.

 ; param is 1+2+$a, literal
 param=1+2+$a

 ; param is 5
 a=3
 b=4
 ;!expr
 param = ($a**2 + $b**2) ** 0.5

 ; to refer to sections
 [Section1]
 lang="Perl"

 [Section2]
 ;!expr
 param="I love " + $CONFIG['Section1']['lang']

Note: since parsing is done in 1-pass, make sure that you define a parameter
first before using it in expressions.

=head2 Merging between sections

Directive !merge is used to merge sections.

 [default]
 repeat=1
 volume=100

 ;!merge default
 [steven]
 file=/home/steven/song1.mp3
 repeat=2

 ;!merge default steven
 [steven, car]
 file=/home/steven/song2.mp3
 volume=30

=head2 Unsupported features

Some INI implementation support other features, and listed below are those
unsupported by Ini::OD, usually because the features are not popular:

=over 4

=item * Line continuation for multiline value

 param=line 1 \
 line 2\
 line 3

Supported by L<Config::IniFiles>. In Ini::OD, use quoting:

 param="line 1 \nline 2\nline 3"

=item * Heredoc syntax for array

 param=<<EOT
 value1
 value2
 EOT

Supported by Config::IniFiles. In Ini::OD, use multiple assignment:

 param=value1
 param=value2

=back


=head1 METHODS


=head1 FUNCTIONS

None are exported by default, but they are exportable.


=head1 FAQ

=head2 Why use INI format for configuration files?

It is popular and familiar to many users. The format is simple to understand
(this cannot be said of other formats like YAML). The simplicity of INI format
also makes it easier to write round trip parser for.

=head2 Were you on drugs?

Sorry, no.


=head1 SEE ALSO

Expression evaluation is done using L<Language::Expr>.

Other INI modules: L<Config::IniFiles>, L<Config::INI>, L<Config::INIPlus>, etc.

Other alternative formats: L<YAML>, L<JSON>, L<Config::General>, XML, etc.

The original blog post/discussion which leads to this module:
http://blogs.perl.org/users/steven_haryanto/2011/09/yaml-vs-ini-again-and-the-plan-for-yet-another-ini-module.html

This module uses L<Log::Any> logging framework.

This module's functions has L<Sub::Spec> specs.

=cut
