#!perl

use 5.010;
use strict;
use warnings;

use Config::IOD;
use File::ShareDir ':ALL';
use Test::Exception;
use Test::More 0.98;

my $dir = dist_dir('IOD-Examples');
diag ".IOD files are at $dir";

my $iod = Config::IOD->new;

my @files = glob "$dir/examples/*.iod";
diag explain \@files;

for my $file (@files) {
    next if $file =~ /TODO-/;

    subtest "file $file" => sub {
        if ($file =~ /invalid-/) {
            dies_ok { $iod->read_file($file) } "dies";
        } else {
            lives_ok { $iod->read_file($file) } "lives";
            # XXX test round-tripness
        };
    }
}

DONE_TESTING:
done_testing;
