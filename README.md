# NAME

Dist::Zilla::Plugin::Keywords - add keywords to metadata in your distribution

# VERSION

version 0.001

# SYNOPSIS

In your `dist.ini`:

    [Keywords]
    keyword = plugin
    keyword = tool
    keywords = development Dist::Zilla

# DESCRIPTION

This plugin adds metadata to your distribution under the `keywords` field.
The [CPAN meta specification](https://metacpan.org/pod/CPAN::Meta::Spec#keywords)
defines this field as:

    A List of keywords that describe this distribution. Keywords must not include whitespace.

# CONFIGURATION OPTIONS

## `keyword`, `keywords`

One or more words to be added as keywords. Can be repeated more than once.
Strings are broken up by whitespace and added as separate words.

# SUPPORT

Bugs may be submitted through [the RT bug tracker](https://rt.cpan.org/Public/Dist/Display.html?Name=Dist-Zilla-Plugin-Keywords)
(or [bug-Dist-Zilla-Plugin-Keywords@rt.cpan.org](mailto:bug-Dist-Zilla-Plugin-Keywords@rt.cpan.org)).
I am also usually active on irc, as 'ether' at `irc.perl.org`.

# SEE ALSO

- [https://metacpan.org/pod/CPAN::Meta::Spec#keywords](https://metacpan.org/pod/CPAN::Meta::Spec#keywords)

# AUTHOR

Karen Etheridge <ether@cpan.org>

# COPYRIGHT AND LICENSE

This software is copyright (c) 2014 by Karen Etheridge.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.
