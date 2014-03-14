use strict;
use warnings FATAL => 'all';

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
# KEYWORDS: foo bar baz
# KEYWORDS: and more here, to be ignored
1;
=pod

=head1 SYNOPSIS

    # KEYWORDS: do not find these

=cut
MODULE
        },
    },
);

$tzil->chrome->logger->set_debug(1);
$tzil->build;

my $json = $tzil->slurp_file('build/META.json');
cmp_deeply(
    $json,
    json(superhashof({
        dynamic_config => 0,
        keywords => [ qw(foo bar baz) ],
    })),
    'metadata is correct',
) or diag 'saw messages:' . join("\n", @{ $tzil->log_messages });


done_testing;
