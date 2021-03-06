#!/usr/local/bin/perl

#  Runs snapper2 to get data for deciding on parameters
#
#  Collects information on
#    run time
#    memory usage
#    filter settings

use strict;

#  merSkips are reordered to balance memory usage of parallel jobs better

my @merSizes      = ( 17, 23 );
my @merSkips;
$merSkips[17]     = [ 0,  1,  2,  3,  4,  5,  6,  8,  9, 12, 16 ];
$merSkips[18]     = [ 0,  1,  2,  3,  4,  5,  6,  9, 13, 17 ];
$merSkips[21]     = [ 0,  1,  2,  3,  4,  5,  6,  7, 11, 16, 20 ];
$merSkips[23]     = [ 0,  1,  2,  3,  4,  5,  6,  7,  8, 12, 17, 22 ];
$merSkips[25]     = [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 13, 19, 24 ];
$merSkips[28]     = [ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 14, 21, 27 ];
my @maxDs         = ( 25 );
my @minHitCov     = ( 0.20 );    # ( 0.00, 0.20, 0.40, 0.60, 0.80 );
my @maskThreshold = ( 5000 ); # ( "-", 5000, 2500, 1500, 1000, 750, 500, 250, 100 );

#  Memory usage
#
#  memory for 18,0  is ~16GB, build only
#  memory for 18,1  is ~ 8GB, build only
#  memory for 18,17 is ~ 1GB, build only
#
#  memory for 23,0  is ~19GB, build only
#  memory for 23,1  is ~10GB, build only
#  memory for 23,22 is ~ 1GB, build only
#
#  memory for 28,0  is ~23GB, build only
#  memory for 28,1  is ~12GB, build only
#
#  Looks like we can divide the memory,0 by the skip, and round up to
#  the next GB, to estimate the amount of memory needed for all skip
#  values.
#
#  Assume searching adds another 4GB.
#
#  
my @memory;
$memory[18] = 16500;
$memory[23] = 19500;
$memory[28] = 23500;



my $tableBuilds = 0;
my $snapperRuns = 0;

my $snapper2 = "/IR/devel/mapping/cds/IR/BRI/snapper2/snapper2";
my $genome   = "/IR/devel/genomics/GENOMES/B34C/B34C.fasta";
my $queries  = "/IR/prod/genomics/walenz/rs-100000.fasta";
my $workDir  = "/IR/prod/genomics/walenz";

$snapper2 = "/Users/bri/cds/IR/BRI/snapper2/snapper2";
$genome   = "/raid/data/0000.fasta";
$queries  = "/raid/data/rs_ch01.fasta";
$workDir  = "/Users/bri/snapper2";



$tableBuilds = scalar(@merSizes) * scalar(@merSkips);
$snapperRuns = $tableBuilds * scalar(@maxDs) * scalar(@minHitCov) * scalar(@maskThreshold);

print STDERR "Computing $tableBuilds tables.\n";
print STDERR "Computing $snapperRuns snapper2 parameter sets.\n";

$tableBuilds = 0;
$snapperRuns = 0;

my $bsub = "bsub -P 00008:HUM_ANNOT:L";
my $host = "-q high_mem";

foreach my $ms (@merSizes) {
    foreach my $mk (@{$merSkips[$ms]}) {
        $tableBuilds++;

        #  Run snapper2 to build the table for this mersize/merskip.
        #  It short circuits if the table already exists, yay!
        #
        my $mem = int($memory[$ms] / ($mk + 1)) + 256;
        my $cmd = "";

        if ($mem < 4000) {
            $host = "-q assembly";
        } else {
            $host = "-q high_mem";
        }

        #$cmd .= "$bsub $host -n 1 -R \"span[hosts=1]select[mem>$mem]rusage[mem=$mem]\" -J S2B_$ms,$mk -o $workDir/posDB-$ms-$mk.lsfout ";
        $cmd .= "$snapper2 -verbose -buildonly";
        $cmd .= " -mersize        $ms";
        $cmd .= " -merskip        $mk";
        $cmd .= " -genomic        $genome";
        $cmd .= " -positions      $workDir/posDB-$ms-$mk";
        $cmd .= " -stats          $workDir/posDB-$ms-$mk.stats";
        $cmd =~ s/\s+/ /g;

        print STDERR "$cmd\n";
        system($cmd);
    }
}


#
#  For all the parameter choices, run.
#

foreach my $ms (@merSizes) {
    foreach my $mk (@{$merSkips[$ms]}) {
        foreach my $d (@maxDs) {
            foreach my $hc (@minHitCov) {
                foreach my $mt (@maskThreshold) {
                    $snapperRuns++;

                    my $nam = "run-$ms-$mk-$d-$hc-$mt";
                    my $mem = int($memory[$ms] / ($mk + 1)) + 256 + 4000;
                    my $cmd = "";

                    if ($mem < 4000) {
                        $host = "-q assembly";
                    } else {
                        $host = "-q high_mem";
                    }

                    #$cmd .= "$bsub $host -n 6 -R \"span[hosts=1]select[mem>$mem]rusage[mem=$mem]\" -J S2R_$snapperRuns -o $workDir/$nam.lsfout ";
                    $cmd .= "$snapper2 -verbose";
                    $cmd .= " -mersize        $ms";
                    $cmd .= " -merskip        $mk";
                    $cmd .= " -maxdiagonal    $d";
                    $cmd .= " -minhitcoverage $hc";
                    $cmd .= " -maskn          $workDir/$ms.ge10 $mt" if ($ms ne "-");
                    $cmd .= " -genomic        $genome";
                    $cmd .= " -queries        $queries";
                    $cmd .= " -positions      $workDir/posDB-$ms-$mk";
                    $cmd .= " -numthreads     2";
                    $cmd .= " -validate       $workDir/$nam.validate";
                    $cmd .= " -output         $workDir/$nam.out";
                    $cmd .= " -stats          $workDir/$nam.stats";
                    $cmd =~ s/\s+/ /g;

                    print STDERR "$cmd\n";
                    system($cmd);
                }
            }
        }
    }
}

print STDERR "Built $tableBuilds tables for $snapperRuns trials.\n";

