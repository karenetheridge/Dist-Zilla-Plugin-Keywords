use strict;
use warnings FATAL => 'all';

use Test::More;
use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Test::Deep;
use Test::Deep::JSON;
use Test::DZil;
use Path::Tiny;

my $preamble = <<'PREAMBLE';
name = DZT-Sample
abstract = Sample DZ Dist
version  = 0.001
author   = E. Xavier Ample <example@example.org>
license  = Perl_5
copyright_holder = E. Xavier Ample

PREAMBLE

foreach my $dist_ini (
    simple_ini(
        [ MetaJSON => ],
        [ Keywords => { keywords => [ qw(foo bar baz) ] } ],
    ),
    $preamble . <<'INI',
[MetaJSON]
[Keywords]
keyword = foo
keyword = bar
keyword = baz
INI
    $preamble . <<'INI',
[MetaJSON]
[Keywords]
keywords = foo bar baz
INI
    $preamble . <<'INI',
[MetaJSON]
[Keywords]
keywords = foo bar
keyword = baz
INI
)
{
    my $tzil = Builder->from_config(
        { dist_root => 't/does_not_exist' },
        {
            add_files => {
                path(qw(source dist.ini)) => $dist_ini,
            },
        },
    );

    $tzil->build;

    my $json = $tzil->slurp_file('build/META.json');
    cmp_deeply(
        $json,
        json(superhashof({
            dynamic_config => 0,
            keywords => [ qw(foo bar baz) ],
        })),
        'metadata is correct',
    );
}

done_testing;
