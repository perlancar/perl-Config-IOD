package Test::Config::IOD;

## no critic (Modules::ProhibitAutomaticExportation)

use 5.010;
use strict;
use warnings;

use Test::Differences;
use Test::Exception;
use Test::More;
use Config::IOD;

use Exporter qw(import);

# AUTHORITY
# DATE
# DIST
# VERSION

our @EXPORT = qw(test_modify_doc);

sub test_modify_doc {
    my $opts;
    if (ref($_[0]) eq 'HASH') {
        $opts = shift;
    } else {
        $opts = {};
    }
    my ($code, $doc1, $doc2, $name) = @_;

    subtest +($name // "test_modify_doc") => sub {
        my $iod = Config::IOD->new;
        my $doc = $iod->read_string($doc1);
        if ($opts->{dies}) {
            dies_ok { $code->($doc) } "dies"
                or return 0;
            return 1;
        } else {
            lives_ok { $code->($doc) } "lives"
                or return 0;
        }
        eq_or_diff $doc->as_string, $doc2, "result";
    };
}

1;
# ABSTRACT: Testing routines for Config::IOD

=head1 FUNCTIONS

=head2 test_modify_doc($code, $doc1, $doc2[, $test_name]) => bool

Parse string C<$doc1> into a L<Config::IOD::Document> object, then run C<<
$code->($doc_obj) >>, then compare C<< $doc_obj->as_string >> with string
C<$doc2>.
