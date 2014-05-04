use strict;
use warnings FATAL => 'all';

use utf8;
use Test::More;
use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Test::Deep;
use Test::Deep::JSON;
use Test::DZil;
use Path::Tiny;

my $tzil = Builder->from_config(
    { dist_root => 't/does_not_exist' },
    {
        add_files => {
            path(qw(source dist.ini)) => simple_ini(
                [ GatherDir => ],
                [ MetaJSON => ],
                [ Keywords => ],
            ),
            path(qw(source lib Foo.pm)) => <<MODULE,
package Foo;
# ABSTRACT: here there be Foo
# here is an irrelevant comment
use utf8;
# KEYWORDS: pi π
1;
MODULE
        },
    },
);

$tzil->chrome->logger->set_debug(1);
$tzil->build;

my $json = path($tzil->tempdir, qw(build META.json))->slurp_raw;
cmp_deeply(
    $json,
    json(superhashof({
        dynamic_config => 0,
        keywords => ['pi', 'π'],
    })),
    'metadata contains keywords',
) or diag 'saw messages:' . join("\n", @{ $tzil->log_messages });

cmp_deeply(
    $tzil->log_messages,
    superbagof('[Keywords] found keyword string in main module: pi π'),
    'we logged the strings we used, with no encoding errors',
) or diag 'got: ', explain $tzil->log_messages;

done_testing;
