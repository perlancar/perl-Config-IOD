#!perl -T

use 5.010;
use strict;
use warnings;

use Test::More 0.96;

use Config::Ini::OnDrugs;

sub test_parse {
    my %args = @_;
    subtest $args{name} => sub {
        my $res;
        my $ini;
        eval { $ini = Config::Ini::OnDrugs->new($args{ini}) };
        my $eval_err = $@;
        if ($args{dies}) {
            ok($eval_err, "dies");
        } else {
            ok(!$eval_err, "doesn't die") or diag $eval_err;
        }
        if (defined $args{num_lines}) {
            is(scalar(@{$ini->{_lines}}), $args{num_lines},
               "num_lines=$args{num_lines}")
                or diag $ini->{_lines};
        }
        if ($args{types}) {
            for my $i (0..@{$args{types}}-1) {
                my $t0 = $ini->{_lines}[$i][1];
                my $t = $args{types}[$i];
                is($t0, $t, "type (line ".($i+1).") = $t") or diag $t0;
            }
        }
        if (defined $args{post_test}) {
            is($res, $args{res}, "result");
        }
    };
}

test_parse(ini => "", num_lines=>0, name=>"empty string");
test_parse(ini => "foo", dies=>1, name=>"unknown line");
test_parse(ini => " ", num_lines=>1, types=>["B"], name=>"blank line");
test_parse(ini => "foo=1", num_lines=>1, types=>["P"], name=>"parameter");
test_parse(ini => "; foo=1", num_lines=>1, types=>["C"], name=>"comment");
test_parse(ini => " #foo=1", num_lines=>1, types=>["C"], name=>"comment 2");
test_parse(ini => ";!foo 1", num_lines=>1, types=>["D"], name=>"directive");
test_parse(ini => "[foo bar]", num_lines=>1, types=>["S"], name=>"section");

DONE_TESTING:
done_testing();
