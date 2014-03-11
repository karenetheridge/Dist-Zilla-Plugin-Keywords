use strict;
use warnings FATAL => 'all';

use Test::More;
use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Dist::Zilla::Plugin::Keywords;

fail('this test is TODO!');

done_testing;
