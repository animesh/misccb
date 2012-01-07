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
open(F,"NC_000913.gbk.0");
open(FF,">test");
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
		if($tr ne ""){push(@comcds,$tr);}}}
	else
		{$l=~s/[A-Za-z]/ /g;$l=~s/\/\=\"\"//g;
                @temp=split(/,/,$l);
		foreach $tr (@temp){$tr=~s/\s+//g;
                	chomp $tr;#print "$tr\n";
                	if($tr ne ""){push(@cds,$tr);}
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
#print $line;
#print @cds;
#print @comcds;
foreach $cds1 (@cds){#print "$cds1\n";
$cds1=~s/\s+//g;
@no1=split(/\.\./,$cds1);$lll=@no1;if($lll eq 2){
$length=@no1[1]-@no1[0]+1;
#print "@no1\n";
$str = substr($line,(@no1[0]-1),$length);
print FF">CDS from @no1[0] to @no1[1]\n";
print FF"$str\n";
}}
foreach $cds2 (@comcds){#print "$cds1\n";
@no1=split(/\.\./,$cds2);$lll=@no1;if($lll eq 2){
$length=@no1[1]-@no1[0]+1;
#print "@no1\n";
$str = substr($line,(@no1[0]-1),$length);
$str=~tr/atgc/tacg/d;
$str = reverse($str);
print FF">Complement CDS from @no1[0] to @no1[1]\n";
print FF"$str\n";
}}
