#!/usr/bin/perl

use strict;

use SAXDriver::XMLParser;

my $handler = CamelHandler->new();
my $parser = SAXDriver::XMLParser->new( Handler => $handler);

my $file = "files/camelids.xml";

$parser->parse($file);

package CamelHandler;

use strict;

sub new {
    my $type = shift;
    return bless {}, $type;
}

my $current_element = '';
my $latin_name = '';
my $common_name = '';

sub start_element {
    no strict 'refs';
    my ($self, $element) = @_;

    my %attrs = %{$element->{Attributes}};
    $current_element = $element->{LocalName};

    if ($current_element eq 'species') {
        $latin_name = $element->{Attributes}->{'name'}->{Value};
    }
    elsif ($current_element eq 'conservation') {
        print $common_name .' (' . $latin_name .') ' . $element->{Attributes}->{'status'}->{Value} . "\n";
    }
    

}

sub end_element {
    my ($self, $element) = @_;

    if ($element->{LocalName} eq 'species') {
        $common_name = undef;
        $latin_name  = undef;
    }
}

sub characters {
    my ($self, $characters) = @_;
    my $text = $characters->{Data};
    $text =~ s/^\s*//;
    $text =~ s/\s*$//;
    return '' unless $text;
    
    if ($current_element eq 'common-name') {
        $common_name .= $text;
    }
}

sub start_document {
# compat only;
}

sub end_document {
# compat only;
}

sub processing_instruction {
# compat only
}

1;
