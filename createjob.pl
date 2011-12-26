#!/usr/bin/perl
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Code base of Animesh Sharma [ sharma.animesh@gmail.com ]

system("ls *.out|grep [0-9] > temp");
open(T,"temp");
while($l=<T>){
	chomp $l;
	$l=~s/\.\.\///;
	push(@tem,$l);
	
}

foreach $f (@tem){
	@tem2=split(/\./,$f);
	$pbsfn=ftcod.@tem2[3].".pbs";
	open(FPBS,">$pbsfn");
	print FPBS"#! /bin/sh -\n";
	print FPBS"#PBS -N \"ftcod@tem2[3]\"\n";
	print FPBS"#PBS -A informatikk\n"; 
	print FPBS"#PBS -l ncpus=1,walltime=240:00:00\n";
	print FPBS"#PBS -l mem=512mb\n";
	print FPBS"#PBS -o ftcod@tem2[3]job.out\n";
	print FPBS"#PBS -e ftcod@tem2[3]job.err\n";
	print FPBS"cd /home/fimm/ii/ash022/ft/ft_cod_p\n";
	print FPBS"./map2genome@tem2[3].pl\n";
	close FPBS;
        $perlfn=map2genome.@tem2[3].".pl";
        open(FPMG,">$perlfn");
 	open(TP,"map2genome.pl");
	while($l=<TP>){
        	chomp $l;
		if($l=~/^\$main_file_pattern/){
			print FPMG"\$main_file_pattern=\"FCRLHFZ01.sff.fna.@tem2[3].out\"\;\n";
		}
		else{
			print FPMG"$l\n";
		}
	}
	close TP;

	system("chmod 755 $perlfn");
	close FPMG;
        system("qsub $pbsfn");
 }

