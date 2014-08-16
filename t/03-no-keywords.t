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
                [ MetaConfig => ],
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
    all(
        # TODO: replace with Test::Deep::notexists($key)
        code(sub { return !exists $_[0]->{keywords} ? 1 : ( 0, 'found keywords key' ) }),
        superhashof({
            dynamic_config => 0,
            x_Dist_Zilla => superhashof({
                plugins => supersetof(
                    {
                        class => 'Dist::Zilla::Plugin::Keywords',
                        config => {
                            'Dist::Zilla::Plugin::Keywords' => { keywords => [] },
                        },
                        name => 'Keywords',
                        version => ignore,
                    },
                ),
            }),
        })
    ),
    'empty keywords field does not appear in metadata',
) or diag 'got distmeta: ', explain $tzil->distmeta;

diag 'saw log messages: ', explain($tzil->log_messages)
    if not Test::Builder->new->is_passing;

done_testing;
