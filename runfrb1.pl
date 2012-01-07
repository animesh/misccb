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

$x=1000;
while($x>366){
	system("perl mlp5.pl nmix 54 ntest.txt");
	open(NEW,"t1.txt");
	while($new=<NEW>){
		$c++;
		@t=split(/\-/,$new);
		if(@t[0] eq "MISCLASSIFICATION"){
			$x=@t[1];
			print "RunMLP-$x\n";
			#if($x<=7){die"RunFRB-$x\n";}
		}
	}
	$c=0;
	close NEW;
}
