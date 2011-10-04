package Config::Ini::OnDrugs;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

# VERSION

use Exporter::Lite;
our @EXPORT_OK = qw();

our %SPEC;

$SPEC{} = {
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

Comment after section name and closing "]" is allowed:

 [Section] ; some comment
 [Section2] #!another comment, directive not allowed and assumed as comment

=head2 Parameter line

Parameter lines specify name value pairs:

 Parameter name = value
   Parameter2=value ; this is not a comment and part of value

Parameter name and value can be quoted:

 "Contains\nnewline" = "\0" ; this is a comment, no directive allowed

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
 bar="2" ; b

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

=item * Deltas

See Config::IniFiles. In Ini::OD, you can put history in comments.

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
