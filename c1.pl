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
$part=400;
#$focds=$file.".cds";$foncds=$file.".noncds";$s=$file.".seq";
open(F,$file);
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
#			print "from join $l\n";
                        }
             $l =~ s/\(/ /g;
             $l =~ s/\)/ /g;
             $l =~ s/join//;
             $l =~ s/CDS//;#print $l;
	     #print "from join $l\n";
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
$line=($line);$line=~s/\///g;1/1;$seql=length($line);
$div=int ($seql/$part);
#print "$file\t$part\t$seql\t$div\n";
foreach $cds1 (@cds){#print "$cds1\n";
$cds1=~s/\s+//g;$cds1=~s/\>//g;$cds1=~s/\<//g;
@no1=split(/\.\./,$cds1);$lll=@no1;if(($lll eq 2) and (@no1[0]=~/[0-9]/) and 
(@no1[1]=~/[0-9]/)){
$length=@no1[1]-@no1[0]+1;
$st=@no1[0];$sp=@no1[1];$cds{$sp}=$st;
#print "@no1\n";
$str = uc(substr($line,(@no1[0]-1),$length));
#print FF">CDS\t[ @no1[0] - @no1[1] ]\n";print FF"$str\n";
$sname="CDS[@no1[0]-@no1[1]]";
push(@seq,$str);push(@seqname,$sname);
}}
foreach $cds2 (@comcds){#print "$cds1\n";
$cds2=~s/\s+//g;$cds2=~s/\>//g;$cds2=~s/\<//g;
@no1=split(/\.\./,$cds2);$lll=@no1;if(($lll eq 2) and (@no1[0]=~/[0-9]/) and 
(@no1[1]=~/[0-9]/)){
$length=@no1[1]-@no1[0]+1;
#print "@no1\n";
$str = substr($line,(@no1[0]-1),$length);
$str=~tr/atgc/tacg/d;1/1;
$str = uc(reverse($str));
$st=@no1[0];$sp=@no1[1];$cds{$st}=$sp;
#print FFFF">cCDS\t[ @no1[0] - @no1[1] ]\n";print FF">cCDS\t[ @no1[0] - @no1[1] ]\n";#print FF">Complement CDS from @no1[0] to @no1[1]\n";print FF"$str\n";
$sname="cCDS[@no1[0]-@no1[1]]";
push(@seq,$str);push(@seqname,$sname);
}}
for($cc1=0;$cc1<=$#t;$cc1++){#print "$cds1\n";
$cds1=@t[$cc1];
#print "$cc1\t$cds1\n";
$cds1=~s/\s+//g;$cds1=~s/\>//g;$cds1=~s/\<//g;
@no1=split(/\.\./,$cds1);$lll=@no1;
	if(($lll eq 2) and (@no1[0]=~/[0-9]/) and (@no1[1]=~/[0-9]/)){
	push(@to,@no1[0]);push(@to,@no1[1]);
	}
}
for($cc1=0;$cc1<($#to-1);$cc1=$cc1+2){#print "$cds1\n";
	$cds1=@to[$cc1];$sp=(@to[($cc1+2)]-1);$st=(@to[($cc1+1)]+1);
	$length=@to[($cc1+2)]-@to[($cc1+1)]-1;
	if($length le 0){$sp=$sp+1;$st=$st-1;
	#print "overlap\t$cc1\t$cds1\t@to[($cc1+2)]-@to[($cc1+1)]\t$length\n";
	$length=$st-$sp+1;
	$str = uc(substr($line,$sp-1,$length));
	#print FFFF">oIntergenic\t[ $sp-$st ]\n";
	$intgen{$sp}={$st};
	#print FFF">Intergenic[ $st-$sp ]\t$length\tOVERLAP\n";
	#print FFF"$str\n";
	}
	else{#print "$cc1\t$cds1\t@to[($cc1+2)]-@to[($cc1+1)]\t$length\n";
	$str = uc(substr($line,(@to[($cc1+1)]),$length));
	$intgen{$sp}={$st};
	#print FFFF">Intergenic\t[ $st-$sp ]\n";print FFF">Intergenic\t[ $st-$sp ]\t$length\n";	print FFF"$str\n";
	$ncdssname="Intergenic[$st-$sp]";
	push(@ncdsseq,$str);push(@ncdsseqname,$ncdssname);
	}
}
#print "@seqname\n\n@ncdsseqname\n\n";
#foreach $k (@fftcds) {print "Coding from N- $k\n";}
#foreach $k (@seqname) {print "cds are- $k\n";}
foreach $k (@ncdsseqname) {print "$k\n";}
