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
        eval { $ini = Config::Ini::OnDrugs->new(str=>$args{ini}) };
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
                my $t0 = $ini->{_lines}[$i][2];
                my $t = $args{types}[$i];
                is($t0, $t, "type (line ".($i+1).") = $t") or diag $t0;
            }
        }
        if (defined $args{post_test}) {
            $args{post_test}->($ini);
        }
    };
}

test_parse(ini => "", num_lines=>0, name=>"empty string");
test_parse(ini => "foo", dies=>1, name=>"unknown line");
test_parse(ini => " ", num_lines=>1, types=>["B"], name=>"blank line");
test_parse(ini => "foo=1", num_lines=>1, types=>["P"], name=>"parameter");
test_parse(ini => "; foo=1", num_lines=>1, types=>["C"], name=>"comment");
test_parse(ini => " #foo=1", num_lines=>1, types=>["C"], name=>"comment 2");
test_parse(ini => ";!defaults", num_lines=>1, types=>["D"], name=>"directive");
test_parse(ini => ";!array 1", dies=>1, name=>"unknown directive");
test_parse(ini => ";! x", num_lines=>1, types=>["C"], name=>"non directive 1");
test_parse(ini => "; !x", num_lines=>1, types=>["C"], name=>"non directive 2");
test_parse(ini => "[foo bar]", num_lines=>1, types=>["S"], name=>"section");

my $ini1 = <<'_';
[section1]
  foo=1
bar = 2; 3
bar 2= "quoted 1"
"bar 3" = "quoted \"2\""


;!defaults section1
[section2]
foo=
bar = 2
bar 2=
baz=element 1
baz=element 2
baz = element 3

[section2][sub1][subsub1]
[section2] [sub2] [subsub1]
_
test_parse(
    ini => $ini1, num_lines=>18,
    name=>"basics 1",
    post_test=>sub {
        my ($ini) = @_;
        #use Data::Dump; dd $ini->{_tree};
        is($ini->get_value("section1", "foo"), "1", "get_value 1");
        is($ini->get_value("section1", "bar"), "2; 3", "get_value 2");
        is($ini->get_value("section1", "bar 3"), 'quoted "2"', "get_value 3");
    },
);

DONE_TESTING:
done_testing();
