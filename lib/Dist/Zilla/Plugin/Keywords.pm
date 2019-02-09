use strict;
use warnings;
package Dist::Zilla::Plugin::Keywords;
# vim: set ts=8 sts=4 sw=4 tw=115 et :
# ABSTRACT: Add keywords to metadata in your distribution
# KEYWORDS: plugin distribution metadata cpan-meta keywords

our $VERSION = '0.008';

use Moose;
with 'Dist::Zilla::Role::MetaProvider',
    'Dist::Zilla::Role::PPI' => { -version => '5.009' };
use Moose::Util::TypeConstraints;
use Encode ();
use namespace::autoclean;

sub mvp_aliases { +{ keyword => 'keywords' } }
sub mvp_multivalue_args { qw(keywords) }

has keywords => (
    is => 'ro', isa => 'ArrayRef[Str]',
    lazy => 1,
    default => sub {
        my $self = shift;
        my @keywords = $self->keywords_from_file($self->zilla->main_module);
        \@keywords;
    },
);

around BUILDARGS => sub
{
    my $orig = shift;
    my $self = shift;

    my $args = $self->$orig(@_);
    if (my $keywords = delete $args->{keywords})
    {
        $args->{keywords} = [ map split(/\s+/, $_), @$keywords ];
    }

    return $args;
};

around dump_config => sub
{
    my ($orig, $self) = @_;
    my $config = $self->$orig;

    $config->{+__PACKAGE__} = {
        keywords => $self->keywords,
        blessed($self) ne __PACKAGE__ ? ( version => $VERSION ) : (),
    };
    return $config;
};

sub metadata
{
    my $self = shift;

    my $keywords = $self->keywords;
    return {
        @$keywords ? ( keywords => $keywords ) : ()
    };
}

sub keywords_from_file
{
    my ($self, $file) = @_;

    my $document = $self->ppi_document_for_file($file);

    my $keywords;
    $document->find(
        sub {
            die if $_[1]->isa('PPI::Token::Comment')
                and ($keywords) = $_[1]->content =~ m/^\s*#+\s*KEYWORDS:\s*(.+)$/m;
        }
    );
    return if not $keywords;

    if (not eval { Dist::Zilla::Role::PPI->VERSION('6.003') })
    {
        # older Dist::Zilla::Role::PPI passes encoded content to PPI
        $keywords = Encode::decode($file->encoding, $keywords, Encode::FB_CROAK);
    }

    $self->log_debug('found keyword string in main module: ' . $keywords);
    return split /\s+/, $keywords;
}

__PACKAGE__->meta->make_immutable;
__END__

=pod

=head1 SYNOPSIS

In your F<dist.ini>:

    [Keywords]
    keyword = plugin
    keyword = tool
    keywords = development Dist::Zilla

Or, in your F<dist.ini>:

    [Keywords]

And in your main module:

    # KEYWORDS: plugin development tool

=head1 DESCRIPTION

This plugin adds metadata to your distribution under the C<keywords> field.
The L<CPAN meta specification|CPAN::Meta::Spec/keywords>
defines this field as:

    A List of keywords that describe this distribution. Keywords must not include whitespace.

=for Pod::Coverage metadata mvp_aliases mvp_multivalue_args keywords_from_file

=head1 CONFIGURATION OPTIONS

=head2 C<keyword>, C<keywords>

One or more words to be added as keywords. Can be repeated more than once.
Strings are broken up by whitespace and added as separate words.

If no configuration is provided, the main module of your distribution is
scanned for the I<first> C<# KEYWORDS:> comment.

=head1 SEE ALSO

=for :list
* L<CPAN::Meta::Spec/keywords>

=cut
