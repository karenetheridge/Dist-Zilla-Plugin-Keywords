use strict;
use warnings FATAL => 'all';

use Test::More;
use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Test::Deep;
use Test::Deep::JSON;
use Test::DZil;
use Path::Tiny;

plan skip_all => 'proper keyword merging depends on CPAN::Meta::Merge and Dist::Zilla support'
    unless $ENV{AUTHOR_TESTING} and eval 'require CPAN::Meta::Merge; 1';

my $tzil = Builder->from_config(
    { dist_root => 't/does_not_exist' },
    {
        add_files => {
            path(qw(source dist.ini)) => simple_ini(
                [ GatherDir => ],
                [ MetaJSON => ],
                [ Keywords => 'from plugin' => { keywords => [qw(foo bar)] } ],
                [ Keywords => 'direct' => { keywords => [qw(bar baz)] } ],
            ),
            path(qw(source lib Foo.pm)) => "package Foo;\n1\n",
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
        keywords => [qw(foo bar baz)],
    })),
    'metadata contains merged keywords',
) or diag 'saw messages:' . join("\n", @{ $tzil->log_messages });

done_testing;
