#!/usr/bin/perl

use strict;

#  Updates the reference POINTER to the latest successful assembly.

my @assemblies;

open(F, "ls POINTERS/*last |");
while (<F>) {
    if ($_ =~ m/^POINTERS\/(.*).last$/) {
        push @assemblies, $1;
    }
}


foreach my $asm (@assemblies) {
    my $reference = "0000-00-00-0000";
    my $last      = "0000-00-00-0000";

    if (-e "POINTERS/$asm.reference") {
        my %ref;
        my @ref;

        $ref{"0000-00-00-0000"}++;

        open(F, "< POINTERS/$asm.reference") or die;
        while (<F>) {
            chomp;
            if (-d "$_/$asm") {
                $ref{$_}++;
            }
        }
        close(F);

        @ref = sort keys %ref;
        $reference = pop @ref;
    }


    if (-e "POINTERS/$asm.last") {
        my %ref;
        my @ref;

        $ref{"0000-00-00-0000"}++;

        open(F, "< POINTERS/$asm.last") or die;
        while (<F>) {
            chomp;
            if (-e "$_/$asm/$asm.qc") {
                $ref{$_}++;
            }
        }
        close(F);

        @ref = sort keys %ref;
        $last = pop @ref;
    }

    if ($reference lt $last) {
        print STDERR "REF\t$reference\tLAST\t$last\tSTALE\t$asm\n";

        open(F, ">> POINTERS/$asm.reference") or die;
        print F "$last\n";
        close(F);
    } else {
        print STDERR "REF\t$reference\tLAST\t$last\t\t$asm\n";
    }
}
