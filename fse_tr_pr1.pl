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

if( @ARGV ne 2){die "\nUSAGE\t\"ProgName MultSeqFile\t\n\n\n";}
$file = shift @ARGV;$top=shift @ARGV;
use Math::Complex;
$pi=pi;
$i=sqrt(-1);
open (F, $file) || die "can't open \"$file\": $!";
sub cv{
	for($c21=0;$c21<=312;$c21++){
	@vec[$c21]=0;
	}
}
sub pc{
	$seq=shift;
	@t1=split(/\s+/,$seq);
	for($c2=0;$c2<=$#t1;$c2++){
		$temp1+=(@t1[$c2]*@vec[$c2]);
		$temp2+=@t1[$c2];
		$temp3+=@vec[$c2];
		$temp4+=(@t1[$c2]**2);
		$temp5+=(@vec[$c2]**2);
	}
	$length=@t1;$N=$length-1;#print "$N\n";
	$temp6=$temp2**2;
	$temp7=$temp3**2;#$t9=sqrt(($temp4-($temp6/$N))*($temp5-($temp7/$N)));
	$temp11=sqrt(($temp4-($temp6/$N))*($temp5-($temp7/$N)));
	if($temp11==0)
	{
		$temp1=0;
	}
	else{
	$temp1=($temp1-(($temp2*$temp3)/$N))/(sqrt(($temp4-($temp6/$N))*($temp5-($temp7/$N))));
	}
	$ccccc=$c111+1;
	$key=$ccccc."_".$temp2."_".$temp3."_".$c1."\t".$seq;
	$sum{$key}=abs($temp1);
	$temp1=0;$temp2=0;$temp3=0;$temp4=0;$temp5=0;$temp6=0;$temp7=0;

}
$c1=0;
while ($line = <F>) {
	chomp $line;
	@t10=split(/\s+/,$line);
	for($c1=0;$c1<=$#t10;$c1++){
		if($c1<=12 and $c1>=0){
			cv;
			for($c22=0;$c22<=12;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=13;
		}
		if($c1<=19 and $c1>=13){
			cv;
			for($c22=13;$c22<=19;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=20;
		}	
		if($c1<=31 and $c1>=20){
			cv;
			for($c22=20;$c22<=31;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=32;
		}	
		
		if($c1<=38 and $c1>=32){
			cv;
			for($c22=32;$c22<=38;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=39;
		}	
		if($c1<=47 and $c1>=39){
			cv;
			for($c22=39;$c22<=47;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=48;
		}	
		
		if($c1<=54 and $c1>=48){
			cv;
			for($c22=48;$c22<=54;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=55;
		}	
		
		if($c1<=84 and $c1>=55){
			cv;
			for($c22=55;$c22<=84;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=85;
		}	
		
		if($c1<=93 and $c1>=85){
			cv;
			for($c22=85;$c22<=93;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=94;
		}	
		
		if($c1<=109 and $c1>=94){
			cv;
			for($c22=94;$c22<=109;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=110;
		}	
		
		if($c1<=116 and $c1>=110){
			cv;
			for($c22=110;$c22<=116;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=117;
		}	
		
		if($c1<=124 and $c1>=117){
			cv;
			for($c22=117;$c22<=124;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=125;
		}	
		
		if($c1<=137 and $c1>=125){
			cv;
			for($c22=125;$c22<=137;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=138;
		}	
		
		if($c1<=145 and $c1>=138){
			cv;
			for($c22=138;$c22<=145;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=146;
		}	
		
		if($c1<=154 and $c1>=146){
			cv;
			for($c22=146;$c22<=154;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=155;
		}	
		
		if($c1<=163 and $c1>=155){
			cv;
			for($c22=155;$c22<=163;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=164;
		}	
		
		if($c1<=192 and $c1>=164){
			cv;
			for($c22=164;$c22<=192;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=193;
		}	
		
		if($c1<=203 and $c1>=193){
			cv;
			for($c22=193;$c22<=203;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=204;
		}	
		
		if($c1<=214 and $c1>=204){
			cv;
			for($c22=204;$c22<=214;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=215;
		}	
		
		if($c1<=227 and $c1>=215){
			cv;
			for($c22=215;$c22<=227;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=228;
		}	
		
		if($c1<=237 and $c1>=228){
			cv;
			for($c22=228;$c22<=237;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=238;
		}	
		
		if($c1<=246 and $c1>=238){
			cv;
			for($c22=238;$c22<=246;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=247;
		}	
		
		if($c1<=256 and $c1>=247){
			cv;
			for($c22=247;$c22<=256;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=257;
		}	
		
		if($c1<=267 and $c1>=257){
			cv;
			for($c22=257;$c22<=267;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=268;
		}	
		
		if($c1<=278 and $c1>=268){
			cv;
			for($c22=268;$c22<=278;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=279;
		}	
		
		if($c1<=285 and $c1>=279){
			cv;
			for($c22=279;$c22<=285;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=286;
		}	
		
		if($c1<=298 and $c1>=286){
			cv;
			for($c22=286;$c22<=298;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=299;
		}	
		
		if($c1<=312 and $c1>=299){
			cv;
			for($c22=299;$c22<=312;$c22++)
			{
				@vec[$c22]=1;
			}
			pc($line);
			$c1=313;
		}	
	}
	$c111++;
	#if($c111>=10){exit;}
}
foreach $q (sort {$sum{$b} <=> $sum{$a}} keys %sum){
	$c3++;@t222=split(/\s+/,$q);@t=split(/\_/,@t222[0]);#$cls{$c3}++;
	#if(($c3<=$top) and ($cls{@t[3]}<=6)){
		#print "$cls{$c3}-@t222[0]\n";
	if(($cls{@t[3]}<=$top)){
		print "$c3\t@t[3]\t$cls{@t[3]}\t\t$sum{$q}\t$q\n";$cls{@t[3]}++;
		}
	}