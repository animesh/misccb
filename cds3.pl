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
if(@ARGV != 1){die "usage:\t ProgName .GBK-File\n";}
$file=shift @ARGV;
$focds=$file.".cds";
$foncds=$file.".noncds";
$s=$file.".seq";
open(F,$file);
open(FF,">$focds");open(FFF,">$foncds");open(FFFF,">$s");
while($l=<F>)
{
             if($l =~ /CDS/){
                if($l =~ /join/)
                        {unless($l =~ /\)/)
                                {
                                        do{
                                        $linenew=<F>;
                                        chomp ($linenew);
                                        $l = $l.$linenew;
                                                }until ($l =~ /\)/)
                                }
                        }
             $l =~ s/\(/ /g;
             $l =~ s/\)/ /g;
             $l =~ s/join//;
             $l =~ s/CDS//;#print $l;
	if($l=~/complement/){$l=~s/[A-Za-z]/ /g;$l=~s/\/\=\"\"//g;
		@temp=split(/,/,$l);foreach $tr (@temp){$tr=~s/\s+//g;
		chomp $tr;#print "$tr\n";
		if($tr ne ""){push(@comcds,$tr);push(@t,$tr);}}}
	else
		{$l=~s/[A-Za-z]/ /g;$l=~s/\/\=\"\"//g;
                @temp=split(/,/,$l);
		foreach $tr (@temp){$tr=~s/\s+//g;
                	chomp $tr;#print "$tr\n";
                	if($tr ne ""){push(@cds,$tr);push(@t,$tr);}
			}
		}}
	if($l=~/^ORIGIN/)
	{		while($ll=<F>)
			{

			$ll=~s/[0-9]//g;
			$ll=~s/\s+//g;
			#$ll=~s/" "//;
			#print $ll;
			chomp $ll;
			$line.=$ll;
			}#$line=~s/\s+//;$line=~s/" "//;
	}
}
$line=uc($line);$line=~s/\///g;
#print $line;
#print @cds;
#print @comcds;
foreach $cds1 (@cds){#print "$cds1\n";
$cds1=~s/\s+//g;
@no1=split(/\.\./,$cds1);$lll=@no1;if($lll eq 2){
$length=@no1[1]-@no1[0]+1;
#print "@no1\n";
$str = uc(substr($line,(@no1[0]-1),$length));
print FFFF">CDS from @no1[0] to @no1[1]\n";
#print FF">CDS from @no1[0] to @no1[1]\n";
#print FF"$str\n";
}}
foreach $cds2 (@comcds){#print "$cds1\n";
@no1=split(/\.\./,$cds2);$lll=@no1;if($lll eq 2){
$length=@no1[1]-@no1[0]+1;
#print "@no1\n";
$str = substr($line,(@no1[0]-1),$length);
$str=~tr/atgc/tacg/d;1/1;
$str = uc(reverse($str));
print FFFF">Complement CDS from @no1[0] to @no1[1]\n";
#print FF">Complement CDS from @no1[0] to @no1[1]\n";
#print FF"$str\n";
}}
for($cc1=0;$cc1<=$#t;$cc1++){#print "$cds1\n";
$cds1=@t[$cc1];print "$cc1\t$cds1\n";
$cds1=~s/\s+//g;$cds1=~s/\>//g;$cds1=~s/\<//g;
@no1=split(/\.\./,$cds1);$lll=@no1;
	if(($lll eq 2) and (@no1[0]=~/[0-9]/) and (@no1[1]=~/[0-9]/)){
	push(@to,@no1[0]);push(@to,@no1[1]);
	}
}
for($cc1=0;$cc1<=$#to;$cc1=$cc1+2){#print "$cds1\n";
	$cds1=@to[$cc1];$sp=(@to[($cc1+2)]-1);$st=(@to[($cc1+1)]+1);
	$length=@to[($cc1+2)]-@to[($cc1+1)]-1;
	if($length le 0){
	print "overlap\t$cc1\t$cds1\t@to[($cc1+2)]-@to[($cc1+1)]\t$length\n";
	$length=0-$length;
	$str = uc(substr($line,(@to[($cc1+2)]),$length));
	print FFFF">Intergenic[ [ $st-$sp ]\t$length\tOverlap\n";
	#print FFF">Intergenic[ $st-$sp ]\t$length\tOVERLAP\n";
	#print FFF"$str\n";
	}
	else{print "$cc1\t$cds1\t@to[($cc1+2)]-@to[($cc1+1)]\t$length\n";
	$str = uc(substr($line,(@to[($cc1+1)]),$length));
	print FFFF">Intergenic[ [ $st-$sp ]\t$length\n";
	#print FFF">Intergenic[ [ $st-$sp ]\t$length\n";
	#print FFF"$str\n";
	}
}
close FF;close FFF;close FFFF;
#@total=split(//,$line);1/1;
#for($cc2=0;$cc2<=$#total;$cc2++){$c=$cc2+1;print FFFF"$c\t@total[$cc2]\n";}close FFFF;
