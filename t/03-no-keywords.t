use strict;
use warnings FATAL => 'all';

use Test::More;
use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Test::Deep;
use Test::DZil;
use Path::Tiny;

my $tzil = Builder->from_config(
    { dist_root => 't/does_not_exist' },
    {
        add_files => {
            path(qw(source dist.ini)) => simple_ini(
                [ GatherDir => ],
                [ Keywords => ],
            ),
            path(qw(source lib Foo.pm)) => "package Foo;\n1\n",
        },
    },
);

$tzil->chrome->logger->set_debug(1);
$tzil->build;

cmp_deeply(
    $tzil->distmeta,
    # TODO: replace with Test::Deep::notexists($key)
    code(sub { return !exists $_[0]->{keywords} ? 1 : ( 0, 'found keywords key' ) }),
    'empty keywords field does not appear in metadata',
) or diag 'saw messages:' . join("\n", @{ $tzil->log_messages });

done_testing;
