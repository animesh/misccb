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
system("ls -1 * > temp");
open F,"temp";
while($l=<F>)
{
	if($l =~ /gz$/)
		{
		chomp $l;
		$ll=$l;
		$ll=~s/\.tar\.gz//;
		print "$l $ll\n";
		system("tar -xvzf $l");
		chdir $ll;
		system("ls");
		system("perl Makefile.PL");
		system("make");
		system("make install");
		#system("pwd > temp2");
			#open FF,"temp2";
			#while($t2=<FF>)
			#{chomp $t2;$t2=~s/$ll//;}
			#chdir $t2;$t2=0;
		chdir "../";
		system("rm -rf $ll");
		}
		#if ($ll)
		#{next;}
}

