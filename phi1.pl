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

#!/usr/bin/perl

if(@ARGV==1){
	$file=shift @ARGV;
}

else{
	die"Program Usage:perl prog_file_name fasta_file_name\n";
}

open F, "$file" or die "Can't open $file : $!";

while($line=<F>){chomp($line);
	@t=split(/\s+/,$line);
	$number=@t[0];
	$dollar=@t[1];
	$playerleft=GAME($number,$dollar);
	#print "$number\t$dollar\t";
}

sub GAME{
	my $N=shift;
	my $D=shift;
	print "$N\t$D\n";
	undef %dvalp;
	for($c=0;$c<$N;$c++){
		$dvalp{$c}=$D;
	}
	$seed=0;
	$Nl=$N;$pg=0;$dvalpt{$pg}=0;undef %dvalpt;
	while(($Nl>1) and ($dvalpt{$pg}<=1) ){
		for($c=$seed;$c<$N;$c++){
			$Nl=1;
			$dvalp{$c}=$D;
		}
	}
}