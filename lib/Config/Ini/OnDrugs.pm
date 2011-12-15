package Config::Ini::OnDrugs;

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
#ABSTRACT: Yet another INI reader/writer (round trip, includes, variables, nest)
__END__

=head1 SYNOPSIS

 # oo interface
 use Config::Ini::OnDrugs;
 my $ini = Config::Ini::OnDrugs->new(file => "file.ini");
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


=head1 DESCRIPTION

B<WARNING: THIS MODULE IS NOT FUNCTIONAL, PLEASE DO NOT USE IT. I AM STILL
REVISING THE SPECIFICATION, IMPLEMENTATION IS STILL VERY MINIMAL/PARTIAL.>

This module provides INI reading/writing class/functions. There are several
other INI modules on CPAN; this one focuses on round trip parsing, includes,
variables. The goal is to provide a usable format for configuration files
suitable for automation (programmatic modification) as well as editing by
humans.

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


=head1 FORMAT SPECIFICATION

Since the INI format does not have any formal specification, here is the
specification for INI as used by this module (from here on: IOD). IOD is an
extended INI format. If you don't use any extended features, your IOD file is a
plain INI file.

A configuration is a text file containing a sequence of lines. Each line is
either a blank line, a comment line, a directive line, a section line, or a
parameter line. Parsing is done line-by-line and in one pass.

=head2 Blank line

A blank line is a line containing zero or more whitespaces only. It is ignored.

=head2 Comment line

A comment line begins with ; or # as it's first nonblank character (note that
some INI parsers do not allow indented comment). The use of ; is preferred over
#, as some INI parsers (like one in PHP 5.3+) do not recognize or warn comments
using #.

=head2 Directive line

A directive line is a special comment line, the comment starts with an
exclamation mark ("!") followed by a directive name and zero or more arguments.
An invalid directive will be ignored and assumed to be a normal command (with
warnings).

 ;!directivename arg ...
 ; !invalid because it contains space

Directives influence parsing and turn on/off features.

Below is the list of known directives:

=head3 !include <PATH>

Include another file, as if the content of the included file is the next lines
in the current file. If PATH is not absolute, it is assumed to be relative to
the current file (or included file). A circular include will cause the parser to
die with an error. Example:

File dir1/a.ini:
 [sectionA]
   [sub1]
   a=1
   ;!include ../dir2/b.ini

File dir2/b.ini:

    b=2
 ;dedent

File dir2/b2.ini:

 c := 2+1

File dir2/b3.ini:

   c=4
 [sectionB]
 c=1

Will parsed, will result in this configuration:

 {
     sectionA => {
         sub1 => {
             a => 1,
             b => 2,
             c => [3, 4],
         },
     },
     sectionB => {
         c => 1,
     },
 }

=head3 !defaults <SECTION> <SECTION2> ...

Specify that from now on, when encountering a new section, fill it with values
from SECTION (and then SECTION2, and so on) unless the values are already
specified.

Example:

 [sect1]
 a=1
 b=2

 ;!defaults sect1

 [sect2]
 a=10

 [sect3]
 c=1

 [sect4]
 a=0
 b:=undef

Will result in:

 {sect1 => {a=>1, b=>2},
  sect2 => {a=>10, b=>2},
  sect3 => {a=>1, b=>2, c=>1},
  sect4 => {a=>0, b=>undef},

Another, more meaty example:

 [-defaults]
 quota=1000
 ftp=1
 shell=1
 mysql=0

 ;!defaults -defaults

 ; double quota for this user
 [user1]
 quota=2000

 ; disable ftp for this user
 [user2]
 ftp=0

 ; all admin users have unlimited quota
 [-admins]
 quota=-1

 ;!defaults -admins

 [admin1]

 ; this admin cannot use shell
 [admin2]
 shell=0

 ;no more defaults
 ;!defaults

Defaults to the same section is ignored.

=head3 !merge

Mode-merging using L<Data::ModeMerge>. Details to be specified later.

=head2 Section line

A section line introduces a section:

 [Section Name]
 ["quoted [] section name"]
 []
 [""]

Comment is allowed at the end. To write a section name with problematic
characters (like "\n", "\0", "]", etc.), use quotes.

To specify nested section, you indent the subsection line relative to section
line (minimum is 1 whitespace):

 [Outer]
   [Inner]
     [Further Inner]
       [a]
       val=1
       [b]
 [c]
 val=2

will result in:

 {Outer => {
     Inner => {
         "Further Inner" => {
             a => {
                 val => 1,
             },
             b => {
             },
         },
     },
  c => {
      val => 1,
  },
 },

Use of tab character is discouraged, but if exists as indentation it will be
counted as 1 whitespace. Parameter line needs to be indented at least as its
section:

 [Section]
   [Subsection]
   a=1
     b=2
   c=3
 d=4
  e=5

In the above example, a, b, and c belong to Subsection will d and e belong to
Section.

Non-contiguous sections are allowed, they will be assumed to be set/add
parameters, e.g.:

 [sect1]
 a=1

 [sect2]
 a=1

 [sect1]
 a=2
 b=3

will result in C<sect1> containing C<a> as [1, 2] and C<b> as 3. However, note:

 [sect1]
 a=1

 ;!defaults sect1
 [sect2]
 d=4

 [sect1]
 b=2

 [sect3]
 c=3

C<sect2> will contain {a=>1, d=>4} since at the point of parsing C<sect2>,
C<sect1> only contains {a=>1}. However, C<sect3> will contain {a=>1, b=>2, c=>3}
since at the point of parsing C<sect3>, C<sect1> already becomes {a=>1, b=>2}.

=head2 Parameter line

Parameter lines specify name value pairs:

 Parameter name = value
   Parameter2=value ; this is not a comment and part of value

Parameter name and value can be quoted:

 "Contains\nnewline" = "\0"

Whitespace before parameter name is allowed and can be used to enter/leave
subsection (see L</"Subsection line">). Whitespaces between the "=" character
are allowed and ignored. Trailing whitespace is allowed and ignored for quoted
values but significant for unquoted values.

 a=1<space>
 b="2"<space>

In the above example, value of C<a> is "1" followed by a space, while value of
C<b> is just "2".

Note that some INI parsers forbid some/all of these whitespaces.

Instead of "=" you can use ":=" to specify expression instead of literal value.
Expression is parsed by L<Language::Expr>. Using expression, you can specify a
null (undefined) value:

 param1 := undef

or alternative of writing arrays. Instead of:

 param=foo
 param=bar

you can write:

 param := ["foo", "bar"]

Using expression you can also specify an array with a single value (not possible
using "=").

 param:=["foo"]

You can also specify an empty array using expression:

 param:=[]

To escape ":" as part of parameter name, use quoting:

 "param:"="literal, not expression"

Normally a parameter line should occur below section line, so that parameter
belongs to the section. But a parameter line is also allowed before section
line, in which it will belong to section called C<DEFAULT> (configurable via the
parser's B<default_section> attribute).

=head2 Quoting

Quoting is done with the double-quote (L<">) character. Known escapes are \',
\", \\, \r (linefeed), \f (formfeed), \$ (literal $), \n (newline), \t (tab), \b
(backspace), \a (bell), \0, octal form ("\0377"), hex form ("\xff") and wide-hex
form (\x{263a}).

Quoting is allowed for section name in section line and for parameter name and
value in parameter line.

=head2 Includes

You can include another file using the !include directive:

 ;!include "/My Home/foo.ini"

See the documentation of the !include directive for more details.

=head2 Variables and calculations

You can use variables and calculations using the expressions.

 ; param is 1+2+$a, literal
 param=1+2+$a

 ; param is 5
 a=3
 b=4
 param := ($a**2 + $b**2) ** 0.5

 ; to refer to sections
 [Section1]
 lang="Perl"

 [Section2]
 param:="I love " + $CONFIG['Section1']['lang']

Note: since parsing is done in 1-pass, make sure that you define a parameter
first before using it in expressions.

=head2 Specifying defaults

To avoid repetition, you can specify defaults in a section and then use the
values in that section for others. See documentation on the !defaults directive
for more details.

=head2 Merging between sections

To be specified later.

=head2 Summary of extended features

Below is a summary of extended features provided by IOD over "standard" INI
format:

=over 4

=item * Round-trip parsing

=item * Quoting

=item * Indented section line, comments after section line

=item * Indented comment line

=item * Whitespaces before parameter name, between =, after parameter value

Some INI parsers do not allow whitespaces at all.

=item * # as comment character

Some strict INI parsers only recognize C<;> as the comment starter. If in doubt,
always use C<;>.

=item * Directives (e.g. include, defaults, merging)

Other INI parsers will see directive line as regular comment line.

=item * Expressions

Other INI parsers will see:

 param:=value

as { "param:" => "value" }.

=back

=head2 Unsupported features

Some INI implementation support other features, and listed below are those
unsupported by IOD, usually because the features are not popular:

=over 4

=item * Line continuation for multiline value

 param=line 1 \
 line 2\
 line 3

Supported by L<Config::IniFiles>. In IOD, use quoting:

 param="line 1 \nline 2\nline 3"

=item * Heredoc syntax for array

 param=<<EOT
 value1
 value2
 EOT

Supported by Config::IniFiles. In IOD, use multiple assignment or expression:

 param=value1
 param=value2

or:

 param:=["value1", "value2"]

=back


=head1 METHODS


=head1 FUNCTIONS

None are exported by default, but they are exportable.


=head1 FAQ

=head2 Why use INI format for configuration files?

It is popular and familiar to many users. The format is simple to understand
(this cannot be said of other formats like YAML). The simplicity of INI format
also makes it easier to write round trip parser for.

=head2 Why use Config::Ini::OnDrugs (the IOD format) over standard INI files?

IOD supports several useful (to me, at least) extended features over "standard"
INI files, see L</"Summary of extended features">.

=head2 Downsides of Config::Ini::OnDrugs (IOD format)?

=over 4

=item * Currently only has Perl parser

INI parsers exist everywhere though, so some of the time you can fallback to
INI.

=item * You need to learn another minilanguage for expressions

It's very similar to Perl though. See L<Language::Expr>.

=item * Expression parser is still slowish

I plan to fix this in the future (e.g. by switching parser to L<Marpa>).

=head2 Why the name? Were you on drugs?

Sorry, no.


=head1 SEE ALSO

Other INI modules: L<Config::IniFiles>, L<Config::INI>, L<Config::INIPlus>, etc.

Other alternative formats: L<YAML>, L<JSON>, L<Config::General>, XML, etc.

The original blog post/discussion which leads to this module:
http://blogs.perl.org/users/steven_haryanto/2011/09/yaml-vs-ini-again-and-the-plan-for-yet-another-ini-module.html

This module uses L<Log::Any> logging framework.

This module's functions has L<Sub::Spec> specs.

=cut
