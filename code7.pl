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
foreach $a (a..z)
{foreach $b (a..z)
{$c=$a.$b;push(@ab,$c);}
}
foreach $c (a..z){$d=$c.X;push(@ab,$d);$d=X.$c;push(@ab,$d);$c="";}
push(@ab,XX);
$tt=@ab;
open (FILENAME,"chapter-01.txt")||die "can't open"  $filename: $!";
while ($line = <FILENAME>) {
$whole=$whole.$line;
lc($whole);
$whole=~tr/^\t\n,(0..9);:![]{}()-_\'\;.?\/`~ /X/;
}
foreach $al (@ab)
{
$cont=$whole=~s/$al/$al/g;
#print "$al\t$cont\n";
push(@ccnt,$cont);}
#foreach $vv (@ccnt) {print "$vv\t";}
for($cc=0;$cc<$tt;$cc++)
{
$tree{$ab[$cc]}=$ccnt[$cc];
}
$XX=$tree{XX};
foreach $alls (a..z){
$c=$alls.a;
$cc=$tree{$c};
$t1=$t1+$cc;
push(@a,$cc);
$c=$alls.b;
$cc=$tree{$c};
$t2=$t2+$cc;
push(@b,$cc);
$c=$alls.c;
$cc=$tree{$c};
$t3=$t3+$cc;
push(@c,$cc);
$c=$alls.d;
$cc=$tree{$c};
$t4=$t4+$cc;
push(@d,$cc);
$c=$alls.e;
$cc=$tree{$c};
$t5=$t5+$cc;
push(@e,$cc);
$c=$alls.f;
$cc=$tree{$c};
$t6=$t6+$cc;
push(@f,$cc);
$c=$alls.g;
$cc=$tree{$c};
$t7=$t7+$cc;
push(@g,$cc);
$c=$alls.h;
$cc=$tree{$c};
$t8=$t8+$cc;
push(@h,$cc);
$c=$alls.i;
$cc=$tree{$c};
$t9=$t9+$cc;
push(@i,$cc);
$c=$alls.j;
$cc=$tree{$c};
$t10=$t10+$cc;
push(@j,$cc);
$c=$alls.k;
$cc=$tree{$c};
$t11=$t11+$cc;
push(@k,$cc);
$c=$alls.l;
$cc=$tree{$c};
$t12=$t12+$cc;
push(@l,$cc);
$c=$alls."m";
$cc=$tree{$c};
$t13=$t13+$cc;
push(@m,$cc);
$c=$alls.n;
$cc=$tree{$c};
$t14=$t14+$cc;
push(@n,$cc);
$c=$alls.o;
$cc=$tree{$c};
$t15=$t15+$cc;
push(@o,$cc);
$c=$alls.p;
$cc=$tree{$c};
$t16=$t16+$cc;
push(@p,$cc);
$c=$alls."q";
$cc=$tree{$c};
$t17=$t17+$cc;
push(@q,$cc);
$c=$alls.r;
$cc=$tree{$c};
$t18=$t18+$cc;
push(@r,$cc);
$c=$alls."s";
$cc=$tree{$c};
$t19=$t19+$cc;
push(@s,$cc);
$c=$alls.t;
$cc=$tree{$c};
$t20=$t20+$cc;
push(@t,$cc);
$c=$alls.u;
$cc=$tree{$c};
$t21=$t21+$cc;
push(@u,$cc);
$c=$alls.v;
$cc=$tree{$c};
$t22=$t22+$cc;
push(@v,$cc);
$c=$alls.w;
$cc=$tree{$c};
$t23=$t23+$cc;
push(@w,$cc);
$c=$alls.x;
$cc=$tree{$c};
$t24=$t24+$cc;
push(@x,$cc);
$c=$alls."y";
$cc=$tree{$c};
$t25=$t25+$cc;
push(@y,$cc);
$c=$alls.z;
$cc=$tree{$c};
$t26=$t26+$cc;
push(@z,$cc);
}
foreach $xs (a..z)
{$c=X.$xs;
$cc=$tree{$c};
push(@Xf,$cc);
$t0=$t0+$cc;
$c=$xs.X;
$cc=$tree{$c};
$t27=$t27+$cc;
push(@sX,$cc);
}
$c=X.X;
$cc=$tree{$c};
$t27=$t27+$cc;
push(@sX,$cc);
$t1=$t1+@Xf[0];$t2=$t2+@Xf[1];$t3=$t3+@Xf[2];$t4=$t4+@Xf[3];$t5=$t5+@Xf[4]; $t6=$t6+@Xf[5];$t7=$t7+@Xf[6];
$t8=$t8+@Xf[7];$t9=$t9+@Xf[8];$t10=$t10+@Xf[9];$t11=$t11+@Xf[10];$t12=$t12+@Xf[11]; $t13=$t13+@Xf[12];
$t14=$t14+@Xf[13];$t15=$t15+@Xf[14];$t16=$t16+@Xf[15]; $t17=$t17+@Xf[16]; $t18=$t18+@Xf[17];
$t19=$t19+@Xf[18]; $t20=$t20+@Xf[19]; $t21=$t21+@Xf[20];$t22=$t22+@Xf[21]; $t23=$t23+@Xf[22];
$t24=$t24+@Xf[23]; $t25=$t25+@Xf[24];$t26=$t26+@Xf[25];
@a1= map (($_+1)/($t1+27), @a); @b1= map (($_+1)/($t2+27) , @b);  @c1= map (($_+1)/($t3+27),@c);
@d1= map (($_+1)/($t4+27),@d);@e1= map (($_+1)/($t5+27),@e);@f1= map (($_+1)/($t6+27),@f);
@g1= map (($_+1)/($t7+27),@g);   @h1= map (($_+1)/($t8+27),@h);@i1= map (($_+1)/($t9+27),@i);
 @j1= map (($_+1)/($t10+27),@j); @k1= map (($_+1)/($t11+27),@k); @l1= map (($_+1)/($t12+27),@l);
@m1= map (($_+1)/($t13+27),@m);@n1= map (($_+1)/($t14+27),@a); @o1= map (($_+1)/($t15+27),@o);
@p1= map (($_+1)/($t16+27),@p); @q1= map (($_+1)/($t17+27), @q);@r1= map (($_+1)/($t18+27),@r);
@s1= map (($_+1)/($t19+27),@s);@t1= map (($_+1)/($t20+27),@t); @u1= map (($_+1)/($t21+27),@u);
 @v1= map (($_+1)/($t22+27) , @v);@w1= map (($_+1)/($t23+27), @w); @x1= map (($_+1)/($t24+27), @x);
 @y1= map (($_+1)/($t25+27), @y);  @z1= map (($_+1)/($t26+27), @z );@sX1= map (($_+1)/($t27+27),@sX );
$length=@a1;
#print "a\tb\tc\t\d\te\tf\tg\th\ti\tj\tk\tl\tm\tn\to\tp\tq\tr\ts\tu\tv\tw\tx\ty\tz\n";
for($bb=0;$bb<$length;$bb++){print "@a1[$bb]\t @b1[$bb]\t@c1[$bb]\t@d1[$bb]\t@e1[$bb]\t@f1[$bb]\t@g1[$bb]\t@h1[$bb]\t@i1[$bb]\t@j1[$bb]\t@k1[$bb]\t@l1[$bb]\t@m1[$bb]\t@n1[$bb]\t@o1[$bb]\t@p1[$bb]\t@q1[$bb]\t@r1[$bb]\t@s1[$bb]\t@t1[$bb]\t@u1[$bb]\t@v1[$bb]\t@w1[$bb]\t@x1[$bb]\t@y1[$bb]\t@z1[$bb]\t@sX1[$bb]\n";
}
$len2=@sX1;
for($bbx=0;$bbx<$len2;$bbx++){print "@sX1[$bbx]\t";}