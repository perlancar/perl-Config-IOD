#!perl -T

use 5.010;
use strict;
use warnings;

use Test::More 0.96;

use Config::IOD;

my $ini = Config::IOD->new;

sub test_parse {
    my %args = @_;
    subtest $args{name} // $args{str} => sub {
        my $res;
        eval { $res = $ini->_parse_quoted($args{str}, 0) };
        my $eval_err = $@;
        if ($args{dies}) {
            ok($eval_err, "dies");
        } else {
            ok(!$eval_err, "doesn't die") or diag $eval_err;
        }
        if (defined $args{res}) {
            is($res, $args{res}, "result");
        }
    };
}

test_parse(str => q[], res=>"");
test_parse(str => q['], res=>"'");
test_parse(str => q["], dies=>1);
test_parse(str => q[\"], res=>'"');
test_parse(str => q[\\], dies=>1);
test_parse(str => q[\\x], dies=>1);
test_parse(str => q[\\xj], dies=>1);
test_parse(str => q[\\08], res=>"\08");
test_parse(str => q[\\z], dies=>1, name=>"unknown escape");
test_parse(str => q[\\a], res=>"\a");
test_parse(str => q[\\0], res=>"\0");
test_parse(str => q[a bC  d ], res=>"a bC  d ");

DONE_TESTING:
done_testing();
