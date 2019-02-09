use strict;
use warnings;

use Test::More 0.88;
use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Test::Deep;
use Test::DZil;
use Path::Tiny;

use Test::Needs 'Dist::Zilla::Plugin::Encoding';

my $tzil = Builder->from_config(
    { dist_root => 'does-not-exist' },
    {
        add_files => {
            path(qw(source dist.ini)) => simple_ini(
                [ GatherDir => ],
                [ MetaConfig => ],
                [ Keywords => ],
                [ Encoding => { filename => 'lib/Foo.pm', encoding => 'ascii' } ],
            ),
            path(qw(source lib Foo.pm)) => <<MODULE,
package Foo;
# ABSTRACT: here there be Foo
# here is an irrelevant comment
# KEYWORDS: oh hai
1;
MODULE
        },
    },
);

$tzil->chrome->logger->set_debug(1);
$tzil->build;

cmp_deeply(
    $tzil->distmeta,
    superhashof({
        dynamic_config => 0,
        keywords => [ qw(oh hai) ],
        x_Dist_Zilla => superhashof({
            plugins => supersetof(
                {
                    class => 'Dist::Zilla::Plugin::Keywords',
                    config => {
                        'Dist::Zilla::Plugin::Keywords' => {
                            keywords => [ qw(oh hai) ],
                        },
                    },
                    name => 'Keywords',
                    version => Dist::Zilla::Plugin::Keywords->VERSION,
                },
                superhashof({
                    class => 'Dist::Zilla::Plugin::Encoding',
                    # TODO
                    # config => {
                    #     'Dist::Zilla::Plugin::Encoding' => {
                    #         filename => 'lib/Foo.pm',
                    #         encoding => 'ascii',
                    #     },
                    # },
                    name => 'Encoding',
                    version => Dist::Zilla::Plugin::Encoding->VERSION,
                }),
            ),
        }),
    }),
    'metadata contains keywords',
) or diag 'got distmeta: ', explain $tzil->distmeta;

cmp_deeply(
    [ grep /^\[Keywords\]/, @{ $tzil->log_messages } ],
    [ '[Keywords] found keyword string in main module: oh hai' ],
    'we logged the strings we used, with no encoding errors',
);

diag 'saw log messages: ', explain($tzil->log_messages)
    if not Test::Builder->new->is_passing;

done_testing;
